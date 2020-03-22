module DependencyMonad
  ( DependencyMonad(..)
  , Options(..)
  , RebuildUnconditionally(..)
  , run
  )
where

import Control.Exception
import Data.Binary
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import Development.Shake
import Development.Shake.Classes hiding (show)
import Development.Shake.Rule
import qualified Lucid
import Text.Megaparsec
import Text.MMark as MMark

import Eval
import Bindings (Bindings(..))
import Output (Output)
import qualified Output
import Parse
import Syntax
import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType Bindings -> FilePath -> Text -> m (Maybe Value)

instance DependencyMonad Action where

  listDirectory = getDirectoryContents

  lookupField ft path key = apply1 (FieldAccessQ ft path key)
  {-# INLINE lookupField #-}

-- | The command-line options.
data Options = Options
  { optTargets :: [FilePath]
  , optRebuildUnconditionally :: Maybe RebuildUnconditionally
  , optDelimiters :: Delimiters
  , optVerbosity :: Verbosity
  , optTimings :: Bool }
  deriving stock ( Show )

data RebuildUnconditionally
  = SomeThings (NonEmpty FilePattern)
  | Everything
  deriving stock ( Show )

run :: Options -> Rules () -> IO ()
run options rules =
  shake (optionsToShakeOptions options) $ do
    readYamlCached <- newCache readYaml
    readMarkdownCached <- newCache readMarkdown
    dependDelimiters <- addOracle pure
    readTemplateCached <- newCache (readTemplate dependDelimiters)
    addBuiltinRule noLint noIdentity
      (fieldAccessRuleRun readYamlCached readMarkdownCached readTemplateCached)
    rules

  where
    optionsToShakeOptions Options{..} =
      shakeOptions
        { shakeRebuild = translateRebuild optRebuildUnconditionally
        , shakeTimings = optTimings
        , shakeVerbosity = optVerbosity
        , shakeThreads = 0
        , shakeVersion = "2" }

    translateRebuild optRebuild =
      case optRebuild of
        Just Everything ->
          [(RebuildNow, "**/*")]
        Just (SomeThings patterns) ->
          NonEmpty.toList $ fmap (RebuildNow,) patterns
        Nothing ->
          []

    readYaml path = do
      need [path]
      YamlValue <$> Yaml.decodeFileThrow path

    readMarkdown path = do
      need [path]
      contents <- liftIO $ Text.readFile path
      let parsed = MMark.parse path contents
      mmark <-
        case parsed of
          Left err -> liftIO $ throwIO (MMarkException err)
          Right mmark -> return mmark
      let yaml = fromMaybe (Yaml.Object Map.empty) (MMark.projectYaml mmark)
      let mb_val = Yaml.parseEither Yaml.parseJSON yaml
      record <-
        case mb_val of
          Left err -> liftIO $ throwIO (Yaml.YamlException err)
          Right (Record hashmap) -> return hashmap
          Right _ -> liftIO $ throwIO $ NotAnObject MarkdownFile path
      let markdownOutput =
            ( Output.toStorable
            . Output.fromBuilder
            . runIdentity
            . Lucid.execHtmlT
            . MMark.render ) mmark
      let record' = Map.insert "body" (Output markdownOutput) record
      return $ MarkdownValue $ Record record'

    readTemplate dependDelimiters templatePath = do
      need [templatePath]
      input <- liftIO $ Text.readFile templatePath
      _ <- dependDelimiters (optDelimiters options)
      parsed <- eitherThrow $
        first (ParseError . errorBundlePretty) $
        parseTemplate (optDelimiters options) templatePath input
      return $ ExecTemplate $ \templateParameters -> do
        (Bindings bindings, output) <- (>>= eitherThrow) $
          first (EvalError . toList) <$>
          runStmtT (for_ parsed evalStatement) templateParameters
        let record = Map.insert "body" (Output (Output.toStorable output)) bindings
        return $ Record record

type instance RuleResult Delimiters = Delimiters

{-# INLINEABLE run #-}

data FieldAccessQ = FieldAccessQ
  { faFileType :: FileType Bindings
  , faFilePath :: FilePath
    -- ^ The path to a file containing key-value pairs. This is assumed to be an
    -- absolute path.
  , faField :: Text
  } deriving stock ( Generic, Show, Eq, Typeable )
    deriving anyclass ( Hashable, Binary, NFData )

type instance RuleResult FieldAccessQ = Maybe Value

-- | This newtype exists solely to help me keep the order of arguments in
-- 'fieldAccessRuleRun' straight.
newtype YamlValue = YamlValue { fromYaml :: Value }
-- | See 'YamlValue'.
newtype MarkdownValue = MarkdownValue { fromMarkdown :: Value }
newtype ExecTemplate = ExecTemplate (Bindings -> Action Value)

fieldAccessRuleRun ::
  (FilePath -> Action YamlValue) ->
  (FilePath -> Action MarkdownValue) ->
  (FilePath -> Action ExecTemplate) ->
  FieldAccessQ ->
  Maybe ByteString ->
  RunMode ->
  Action (RunResult (Maybe Value))
fieldAccessRuleRun getYaml getMarkdown getTemplate fa@FieldAccessQ{..} mb_stored mode = do
  mb_record <-
    case faFileType of
      YamlFile -> fromYaml <$> getYaml faFilePath
      MarkdownFile -> fromMarkdown <$> getMarkdown faFilePath
      TemplateFile bindings -> do
        ExecTemplate execTemplate <- getTemplate faFilePath
        execTemplate bindings
  val <-
    case mb_record of
      Record hashMap ->
        return $ Map.lookup faField hashMap
      _ ->
        liftIO $ throwIO $ NotAnObject faFileType faFilePath
  case mb_stored of
    Nothing -> do
      putVerbose (show fa <> ": first run")
      return $ RunResult
        ChangedRecomputeDiff
        (toStrict (encode (hash val)))
        val
    Just stored -> do
      let previous_hash = decode (toLazy stored)
      let (new_hash, did_change) =
            case mode of
              RunDependenciesSame ->
                (previous_hash, ChangedNothing)
              RunDependenciesChanged ->
                let changed =
                      if new_hash == previous_hash
                        then ChangedRecomputeSame else ChangedRecomputeDiff
                in (hash val, changed)
      putVerbose (show fa <> ": " <> show did_change <> " " <> show new_hash)
      return $ RunResult
        did_change
        (toStrict (encode new_hash))
        val

{-# INLINE fieldAccessRuleRun #-}

data NotAnObject = NotAnObject (FileType Bindings) FilePath
  deriving stock ( Show, Typeable )

instance Exception NotAnObject

newtype MMarkException = MMarkException (ParseErrorBundle Text MMarkErr)
  deriving stock ( Show, Typeable )

instance Exception MMarkException where
  displayException (MMarkException err) =
    errorBundlePretty err

data TemplateQ = TemplateQ
  { templateParameters :: Bindings
  , templatePath :: FilePath }
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( Hashable, NFData, Typeable, Binary )

type instance RuleResult TemplateQ = (Bindings, Output)

eitherThrow :: (MonadIO m, Exception e) => Either e a -> m a
eitherThrow = either (liftIO . throwIO) pure

newtype TemplateParseError =
  ParseError String
  deriving newtype Show

instance Exception TemplateParseError where
  displayException (ParseError err) = err

newtype TemplateEvalError = EvalError [Problem]
  deriving stock Show

instance Exception TemplateEvalError

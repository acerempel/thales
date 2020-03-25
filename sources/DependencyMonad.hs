module DependencyMonad
  ( DependencyMonad(..)
  , Options(..)
  , Verbosity(..)
  , RebuildUnconditionally(..)
  , templateExtension
  , toShakeOptions
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
import Development.Shake.FilePath
import Development.Shake.Rule
import qualified Lucid
import qualified System.Directory as System
import System.IO hiding (print)
import Text.Megaparsec
import Text.MMark as MMark

import Eval
import Bindings (Bindings(..))
import qualified Bindings
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
  , optTimings :: Bool
  , optCacheDirectory :: FilePath }
  deriving stock ( Show )

data RebuildUnconditionally
  = SomeThings (NonEmpty FilePattern)
  | Everything
  deriving stock ( Show, Eq )

run :: Options -> IO ()
run options =
  shake (toShakeOptions options) $ do
    want (optTargets options)
    builtinRules options
    fileRules options

{-# INLINEABLE run #-}

toShakeOptions :: Options -> ShakeOptions
toShakeOptions Options{..} =
      shakeOptions
        { shakeRebuild = translateRebuild optRebuildUnconditionally
        , shakeTimings = optTimings
        , shakeVerbosity = optVerbosity
        , shakeFiles = optCacheDirectory
        , shakeThreads = 0
        , shakeVersion = "2" }

  where
    translateRebuild optRebuild =
      case optRebuild of
        Just Everything ->
          [(RebuildNow, "**/*")]
        Just (SomeThings patterns) ->
          NonEmpty.toList $ fmap (RebuildNow,) patterns
        Nothing ->
          []

{-# INLINE toShakeOptions #-}

fileRules :: Options -> Rules ()
fileRules options = do
    (`elem` optTargets options) ?> \targetPath -> do
      let templatePath = targetPath <.> templateExtension
      val <- lookupField (TemplateFile Bindings.empty) templatePath "body"
      case val of
        Just (Output outp) -> do
          absTarget <- liftIO $ System.makeAbsolute targetPath
          putInfo $ "Writing result of " <> templatePath <> " to " <> absTarget
          liftIO $ withBinaryFile targetPath WriteMode $ \hdl -> do
            hSetBuffering hdl (BlockBuffering Nothing)
            Output.write hdl (Output.fromStorable outp)
        _ ->
          error "Oh no!!!"

{-# INLINE fileRules #-}

templateExtension :: String
templateExtension = "template"

builtinRules :: Options -> Rules ()
builtinRules options = do
    readYamlCached <- newCache readYaml
    readMarkdownCached <- newCache readMarkdown
    dependDelimiters <- addOracle pure
    readTemplateCached <- newCache (readTemplate dependDelimiters)
    addBuiltinRule noLint noIdentity
      (fieldAccessRuleRun readYamlCached readMarkdownCached readTemplateCached)

  where
    readYaml path = do
      need [path]
      pathAbs <- liftIO $ System.makeAbsolute path
      putInfo $ "Reading YAML from " <> pathAbs
      YamlValue <$> Yaml.decodeFileThrow path

    readMarkdown path = do
      need [path]
      pathAbs <- liftIO $ System.makeAbsolute path
      putInfo $ "Reading Markdown from " <> pathAbs
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
      pathAbs <- liftIO $ System.makeAbsolute templatePath
      putInfo $ "Reading template from " <> pathAbs
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

{-# INLINE builtinRules #-}

type instance RuleResult Delimiters = Delimiters

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
      case fa of
        FieldAccessQ (TemplateFile _) path "body" ->
          when (did_change == ChangedNothing) $ do
            pathAbs <- liftIO $ System.makeAbsolute path
            putInfo $ "Template body of" <> pathAbs <> " is unchanged"
        _ -> pure ()
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

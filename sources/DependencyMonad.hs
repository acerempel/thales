{-# LANGUAGE UndecidableInstances #-}
module DependencyMonad
  ( DependencyMonad(..)
  , rules
  , run
  )
where

import Control.Exception
import Data.Binary
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text.IO as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import qualified Data.Yaml as Yaml
import Development.Shake
import Development.Shake.Classes hiding (show)
import Development.Shake.Rule
import qualified Lucid
import qualified System.Directory as System
import System.IO hiding (print)
import Text.Megaparsec
import Text.MMark as MMark
import qualified Text.Show as Sh

import Configuration
import Display
import Eval
import qualified Output
import Parse
import Problem
import Syntax
import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType -> FilePath -> Text -> m (Maybe Value)

  listFields :: FileType -> FilePath -> m [Name]

instance DependencyMonad Action where

  listDirectory = getDirectoryContents
  {-# INLINE listDirectory #-}

  lookupField ft path key = apply1 (FieldAccessQ ft path key)
  {-# INLINE lookupField #-}

  listFields ft path = apply1 (FieldListQ ft path)

run :: Options -> IO ()
run options =
  shake (toShakeOptions options) $ rules options

rules :: Options -> Rules ()
rules options@Options{..} = do
    thingsToBuild <- liftIO $ traverse expand optTemplates
    let specifiedThingsToBuild =
          map (specify options) thingsToBuild
        targetToSourceMap =
          createTargetToSourceMap specifiedThingsToBuild
    when (optVerbosity >= Info && optRebuildUnconditionally == Just Everything) $
      liftIO $ hPutStrLn stderr "Rebuilding all targets."
    when (optVerbosity >= Verbose) $ liftIO $ do
      hPrint stderr options
      hPrint stderr targetToSourceMap
    builtinRules
    fileRules targetToSourceMap
    want $ Map.keys targetToSourceMap

{-# INLINE rules #-}

fileRules :: HashMap TargetPath (ThingToBuild Identity SourcePath) -> Rules ()
fileRules targetToSourceMap = do
    (`Map.member` targetToSourceMap) ?> \targetPath -> do
      let thingToBuild = fromJust $ Map.lookup targetPath targetToSourceMap
          templatePath = buildWhat thingToBuild
          delimiters = runIdentity (buildDelimiters thingToBuild)
      val <- lookupField (TemplateFile delimiters Map.empty) templatePath specialBodyField
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

builtinRules :: Rules ()
builtinRules = do
    readYamlCached <- newCache readYaml
    readMarkdownCached <- newCache readMarkdown
    readTemplateCached <- newCache readTemplate
    addBuiltinRule noLint noIdentity
      (fieldAccessRuleRun readYamlCached readMarkdownCached readTemplateCached)
    addBuiltinRule noLint noIdentity
      (fieldListRuleRun readYamlCached readMarkdownCached readTemplateCached)

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
          Right (Record (Concrete hashmap)) -> return hashmap
          Right _ -> liftIO $ throwIO $ NotAnObject MarkdownFile path
      let markdownOutput =
            ( Output.toStorable
            . Output.fromBuilder
            . runIdentity
            . Lucid.execHtmlT
            . MMark.render ) mmark
      let record' = Map.insert specialBodyField (Output markdownOutput) record
      return $ MarkdownValue $ Record $ Concrete record'

    readTemplate (templatePath, delimiters) = do
      need [templatePath]
      pathAbs <- liftIO $ System.makeAbsolute templatePath
      putInfo $ "Reading template from " <> pathAbs
      input <- liftIO $ Text.readFile templatePath
      parsedTemplate <- eitherThrow $
        first (ParseError . errorBundlePretty) $
        parseTemplate delimiters templatePath input
      return $ ExecTemplate $ \templateParameters -> do
        (bindings, output) <- (>>= eitherThrow) $
          first (EvalError . toList) <$>
          evalTemplate parsedTemplate templateParameters
        let record = Map.insert specialBodyField (Output (Output.toStorable output)) bindings
        return $ Record (Concrete record)

{-# INLINE builtinRules #-}

-- | The field we put the output of a template or the body of a markdown file
-- in.
specialBodyField :: Text
specialBodyField = "body"

data FieldAccessQ = FieldAccessQ
  { faFileType :: FileType
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
-- | See 'YamlValue'. Also, note that this is essentially equivalent to
-- '[Statement]', i.e. it is the result of parsing a template — we've just
-- partially applied @'traverse' 'evalStatement'@ or whatever. (I forget why I
-- did it this way — just for symmetry with 'readMarkdownCached' etc.?)
newtype ExecTemplate = ExecTemplate (Bindings -> Action Value)

fieldAccessRuleRun ::
  (FilePath -> Action YamlValue) ->
  (FilePath -> Action MarkdownValue) ->
  ((FilePath, Delimiters) -> Action ExecTemplate) ->
  FieldAccessQ ->
  Maybe ByteString ->
  RunMode ->
  Action (RunResult (Maybe Value))
fieldAccessRuleRun getYaml getMarkdown getTemplate fa@FieldAccessQ{..} mb_stored mode = do
  mb_record <-
    case faFileType of
      YamlFile -> fromYaml <$> getYaml faFilePath
      MarkdownFile -> fromMarkdown <$> getMarkdown faFilePath
      TemplateFile delimiters bindings -> do
        ExecTemplate execTemplate <- getTemplate (faFilePath, delimiters)
        execTemplate bindings
  val <-
    case mb_record of
      Record (Concrete hashMap) ->
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
        FieldAccessQ (TemplateFile _ _) path field | field == specialBodyField ->
          when (did_change == ChangedNothing) $ do
            pathAbs <- liftIO $ System.makeAbsolute path
            putInfo $ "Template body of" <> pathAbs <> " is unchanged"
        _ -> pure ()
      return $ RunResult
        did_change
        (toStrict (encode new_hash))
        val

{-# INLINE fieldAccessRuleRun #-}

data FieldListQ = FieldListQ
  { flFileType :: FileType
  , flFilePath :: FilePath
  } deriving stock ( Generic, Show, Eq, Typeable )
    deriving anyclass ( Hashable, Binary, NFData )

type instance RuleResult FieldListQ = [Name]

fieldListRuleRun ::
  (FilePath -> Action YamlValue) ->
  (FilePath -> Action MarkdownValue) ->
  ((FilePath, Delimiters) -> Action ExecTemplate) ->
  FieldListQ ->
  Maybe ByteString ->
  RunMode ->
  Action (RunResult [Name])
fieldListRuleRun getYaml getMarkdown getTemplate FieldListQ{..} mb_stored mode = do
  case mb_stored of
    Nothing -> do
      fields <- rebuild
      return $ RunResult ChangedRecomputeDiff (toStrict (encode fields)) fields
    Just stored -> do
      let previous = decode (toLazy stored)
      case mode of
        RunDependenciesSame ->
          return $ RunResult ChangedNothing stored previous
        RunDependenciesChanged -> do
          fields <- rebuild
          return $ if fields == previous
             then RunResult ChangedRecomputeSame stored fields
             else RunResult ChangedRecomputeDiff (toStrict (encode fields)) fields
 where
  rebuild = do
    mb_record <-
      case flFileType of
        YamlFile -> fromYaml <$> getYaml flFilePath
        MarkdownFile -> fromMarkdown <$> getMarkdown flFilePath
        TemplateFile delimiters bindings -> do
          ExecTemplate execTemplate <- getTemplate (flFilePath, delimiters)
          execTemplate bindings
    let hasBodyField =
          case flFileType of
            YamlFile -> False
            MarkdownFile -> True
            TemplateFile {} -> True
    case mb_record of
      Record (Concrete hashMap) -> do
        let keys = coerce (Map.keys hashMap)
        return $
          if hasBodyField
             then Name specialBodyField : keys
             else keys
      _ ->
        liftIO $ throwIO $ NotAnObject flFileType flFilePath


data NotAnObject = NotAnObject FileType FilePath
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

instance Sh.Show TemplateEvalError where
  show _ = "problems" -- TODO

instance Exception TemplateEvalError where
  displayException (EvalError problems) =
    renderString
      (layoutPretty
        (LayoutOptions (AvailablePerLine 80 0.7))
        (vsep (punctuate mempty (map display problems))))

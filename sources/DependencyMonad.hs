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
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.Yaml as Yaml
import Development.Shake
import Development.Shake.Classes hiding (show)
import Development.Shake.Rule
import qualified Lucid
import qualified System.Directory as System
import System.IO hiding (print)
import Text.Megaparsec
import Text.MMark as MMark

import Configuration
import Error
import Eval
import Eval.Problem
import Output (Output)
import qualified Output
import Parse
import Syntax
import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType -> FilePath -> Text -> m (Maybe Value)

  listFields :: FileType -> FilePath -> m [Name]

  getBody :: FileType -> FilePath -> m Output

instance DependencyMonad Action where

  listDirectory = getDirectoryContents

  lookupField ft path key = apply1 (FieldAccessQ (DocInfo ft path) key)

  listFields ft path = apply1 (FieldListQ (DocInfo ft path))

  getBody ft path = apply1 (GetBodyQ (DocInfo ft path))

run :: Options -> IO ()
run options = do
  let runRules = shake (toShakeOptions options) $ rules options
  runRules `catch` \exc@ShakeException{} -> do
    let layout = LayoutOptions (AvailablePerLine 80 0.7)
        doc = fuse Shallow $ displayShakeException exc
        docStream = reAnnotateS markupToAnsi $ layoutPretty layout doc
    renderIO stderr docStream

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

fileRules :: HashMap TargetPath (ThingToBuild Identity SourcePath) -> Rules ()
fileRules targetToSourceMap = do
    (`Map.member` targetToSourceMap) ?> \targetPath -> do
      let thingToBuild = fromJust $ Map.lookup targetPath targetToSourceMap
          templatePath = buildWhat thingToBuild
          delimiters = runIdentity (buildDelimiters thingToBuild)
      let addBaseTemplate base (tplf, tplpth) =
            let inner = LoadedDoc (DocInfo tplf tplpth)
            in (TemplateFile delimiters (Map.singleton "content" inner), base)
      outp <- uncurry getBody $ foldr
        addBaseTemplate
        (TemplateFile delimiters Map.empty, templatePath)
        (buildBaseTemplates thingToBuild)
      absTarget <- liftIO $ System.makeAbsolute targetPath
      putInfo $ "Writing result of " <> templatePath <> " to " <> absTarget
      liftIO $ withBinaryFile targetPath WriteMode $ \hdl -> do
        hSetBuffering hdl (BlockBuffering Nothing)
        Output.write hdl outp

builtinRules :: Rules ()
builtinRules = do
    readTemplateCached <- newCache readTemplate
    readDocumentCached <- newCache (readDocument readTemplateCached)
    addBuiltinRule noLint noIdentity
      (fieldAccessRuleRun readDocumentCached)
    addBuiltinRule noLint noIdentity
      (fieldListRuleRun readDocumentCached)
    addBuiltinRule noLint noIdentity (getBodyRuleRun readDocumentCached)

  where
    readDocument :: ((FilePath, Delimiters) -> Action ParsedTemplate) -> DocumentInfo -> Action Document
    readDocument readTemplateCached DocInfo{..} = do
      need [docFilePath]
      case docFileType of
        YamlFile ->
          readYaml docFilePath
        MarkdownFile ->
          readMarkdown docFilePath
        TemplateFile delimiters bindings -> do
          parsedTemplate <- readTemplateCached (docFilePath, delimiters)
          execTemplate parsedTemplate bindings

    readYaml path = do
      pathAbs <- liftIO $ System.makeAbsolute path
      putInfo $ "Reading YAML from " <> pathAbs
      -- TODO wrap this in our own exception type
      Document Nothing <$> Yaml.decodeFileThrow path

    readMarkdown path = do
      pathAbs <- liftIO $ System.makeAbsolute path
      putInfo $ "Reading Markdown from " <> pathAbs
      contents <- liftIO $ Text.readFile path
      mmark <-
        case MMark.parse path contents of
          Left err -> liftIO $ throwIO (MarkdownParseError err)
          Right mmark -> return mmark
      let yaml = fromMaybe (Yaml.Object Map.empty) (MMark.projectYaml mmark)
      record <-
        case Yaml.parseEither Yaml.parseJSON yaml of
          Left err -> liftIO $ throwIO (MarkdownYamlParseError path (Yaml.YamlException err))
          Right (Record hashmap) -> return hashmap
          Right _ -> liftIO $ throwIO $ NotAnObject MarkdownFile path
      let markdownOutput =
            ( Output.fromBuilder . runIdentity
            . Lucid.execHtmlT . MMark.render )
            mmark
      return $ Document (Just markdownOutput) record

    readTemplate (templatePath, delimiters) = do
      pathAbs <- liftIO $ System.makeAbsolute templatePath
      putInfo $ "Reading template from " <> pathAbs
      input <- liftIO $ Text.readFile templatePath
      eitherThrow $
        first (TemplateParseError . errorBundlePretty) $
        parseTemplate delimiters templatePath input

    execTemplate parsedTemplate templateParameters = do
      (bindings, output) <- (>>= eitherThrow) $
        first (TemplateEvalError . toList) <$>
        evalTemplate parsedTemplate templateParameters
      return $ Document (Just output) bindings

{-| A document loaded from some file, containing some key-value pairs (perhaps
none) and (optionally) a document body. YAML documents, Markdown documents, and templates
(once they have been parsed and evaluated) are all represented this way.-}
data Document = Document
  { documentBody :: Maybe Output
  , documentFields :: HashMap Text Value }

{-
-- I wanted to use a GADT to represent all the different document-oriented
-- queries, and share code, but this didn't work, because the key type – the
-- GADT – had to be polymorphic, but Shake needs its keys to be 'Typeable',
-- which means they must be monotypes.
data DocumentQ a = DocQ
  { docInfo :: DocumentInfo
  , docQuery :: DocumentQuery a }

type instance RuleResult (DocumentQ result) = result

data DocumentQuery result where
  FieldAccess :: Text -> DocumentQuery (Maybe Value)
  ListFields :: DocumentQuery [Name]
  GetBody :: DocumentQuery Output
-}

data FieldAccessQ = FieldAccessQ
  { faDocInfo :: DocumentInfo
  , faField :: Text
  } deriving stock ( Generic, Show, Eq, Typeable )
    deriving anyclass ( Hashable, Binary, NFData )

type instance RuleResult FieldAccessQ = Maybe Value

fieldAccessRuleRun ::
  (DocumentInfo -> Action Document) ->
  FieldAccessQ ->
  Maybe ByteString ->
  RunMode ->
  Action (RunResult (Maybe Value))
fieldAccessRuleRun getDocument FieldAccessQ{..} mb_stored mode = do
  Document{ documentFields } <- getDocument faDocInfo
  let val = Map.lookup faField documentFields
  case mb_stored of
    Nothing -> do
      let encoded = toStrict (encode (hash val))
      return $ RunResult ChangedRecomputeDiff encoded val
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
      let encoded = toStrict (encode new_hash)
      return $ RunResult did_change encoded val

newtype FieldListQ = FieldListQ { flDocInfo :: DocumentInfo}
  deriving newtype ( Show, Eq, Binary, Hashable, NFData )

type instance RuleResult FieldListQ = [Name]

fieldListRuleRun ::
  (DocumentInfo -> Action Document) ->
  FieldListQ ->
  Maybe ByteString ->
  RunMode ->
  Action (RunResult [Name])
fieldListRuleRun getDocument FieldListQ{..} mb_stored mode = do
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
    Document{ documentFields } <- getDocument flDocInfo
    -- I don't know that HashMap.keys returns the keys in any particular order
    -- ... let us sort them, to guard against potential spurious rebuilds.
    return $ coerce (sort (Map.keys documentFields))

newtype GetBodyQ = GetBodyQ { gbDocInfo :: DocumentInfo }
  deriving newtype ( Show, Eq, Binary, Hashable, NFData )

type instance RuleResult GetBodyQ = Output

getBodyRuleRun ::
  (DocumentInfo -> Action Document) ->
  GetBodyQ ->
  Maybe ByteString ->
  RunMode ->
  Action (RunResult Output)
getBodyRuleRun getDocument GetBodyQ{..} _mb_stored mode = do
  Document{ documentBody } <- getDocument gbDocInfo
  case documentBody of
    Nothing ->
      error "not a document" -- TODO throw exception
    Just body -> do
      let did_change =
            case mode of
              RunDependenciesSame -> ChangedNothing
              RunDependenciesChanged -> ChangedRecomputeDiff -- conservative
      return $ RunResult did_change mempty body

eitherThrow :: (MonadIO m, Exception e) => Either e a -> m a
eitherThrow = either (liftIO . throwIO) pure

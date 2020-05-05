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
import Development.Shake.FilePath
import Development.Shake.Rule
import System.IO hiding (print)
import Text.Megaparsec

import Configuration
import DependencyMonad.Class
import Error
import Eval
import Eval.Problem
import Output (Output)
import qualified Output
import Parse
import Syntax
import Value

newtype RebuildM a = Rebuild (ReaderT Env Action a)
  deriving newtype ( Functor, Monad, Applicative, MonadIO )

data Env = RebuildEnv
  { envSourceDirectory :: FilePath
  }

runRebuild :: FilePath -> RebuildM a -> Action a
runRebuild sourceDir (Rebuild r) =
  runReaderT r (RebuildEnv sourceDir)

getSourceDirectory :: RebuildM FilePath
getSourceDirectory = Rebuild (asks envSourceDirectory)

liftAction :: Action a -> RebuildM a
liftAction = Rebuild . lift

instance DependencyMonad RebuildM where

  listDirectory dir = do
    sourceDir <- getSourceDirectory
    liftAction (getDirectoryContents (sourceDir </> dir))

  lookupField ft path key = liftAction $ apply1 (FieldAccessQ (DocInfo ft path) key)

  listFields ft path = liftAction $ apply1 (FieldListQ (DocInfo ft path))

  getBody ft path = liftAction $ apply1 (GetBodyQ (DocInfo ft path))

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
    when (optVerbosity >= Info && optRebuildUnconditionally == Just Everything) $
      liftIO $ hPutStrLn stderr "Rebuilding all targets."
    when (optVerbosity >= Verbose) $ liftIO $ hPrint stderr options
    builtinRules optInputDirectory optDelimiters
    fileRules options

fileRules :: Options -> Rules ()
fileRules options@Options{..} = do
    targetToSourceMap <- liftIO $ specify options
    when (optVerbosity >= Verbose) $ liftIO $ hPrint stderr targetToSourceMap
    want $ Map.keys targetToSourceMap
    (`Map.member` targetToSourceMap) ?> \targetPath -> do
      let thingToBuild = fromJust $ Map.lookup targetPath targetToSourceMap
          templatePath = buildSourceFile thingToBuild
      let addBaseTemplate base (tplf, tplpth) =
            let inner = LoadedDoc (DocInfo tplf tplpth)
            in (TemplateFile (Map.singleton "content" inner), base)
      outp <-
        runRebuild optInputDirectory $
        uncurry getBody $
        foldr
          addBaseTemplate
          (TemplateFile Map.empty, templatePath)
          (buildBaseTemplates thingToBuild)
      putInfo $ "Writing result of " <> templatePath <> " to " <> targetPath
      liftIO $ withBinaryFile targetPath WriteMode $ \hdl -> do
        hSetBuffering hdl (BlockBuffering Nothing)
        Output.write hdl outp

data AskDelimiters = AskDelimiters
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( Hashable, NFData, Binary )

type instance RuleResult AskDelimiters = Delimiters

builtinRules :: FilePath -> Delimiters -> Rules ()
builtinRules sourceDirectory delimiters = do
    _ <- addOracle (\AskDelimiters -> pure delimiters)
    readTemplateCached <- newCache readTemplate
    readDocumentCached <- newCache (readDocument readTemplateCached)
    addBuiltinRule noLint noIdentity
      (fieldAccessRuleRun readDocumentCached)
    addBuiltinRule noLint noIdentity
      (fieldListRuleRun readDocumentCached)
    addBuiltinRule noLint noIdentity (getBodyRuleRun readDocumentCached)

  where
    readDocument :: (FilePath -> Action ParsedTemplate) -> DocumentInfo -> Action Document
    readDocument readTemplateCached DocInfo{docFileType, docFilePath = relativeDocFilePath} = do
      let docFilePath = sourceDirectory </> relativeDocFilePath
      need [docFilePath]
      case docFileType of
        YamlFile ->
          readYaml docFilePath
        TemplateFile bindings -> do
          parsedTemplate <- readTemplateCached docFilePath
          runRebuild sourceDirectory $ execTemplate parsedTemplate bindings

    readYaml path = do
      putInfo $ "Reading YAML from " <> path
      -- TODO wrap this in our own exception type
      Document Nothing <$> Yaml.decodeFileThrow path

    readTemplate templatePath = do
      _delimiters <- askOracle AskDelimiters
      putInfo $ "Reading template from " <> templatePath
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

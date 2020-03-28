{-# LANGUAGE UndecidableInstances #-}
module DependencyMonad
  ( DependencyMonad(..)
  , Options(..)
  , Verbosity(..)
  , ThingToBuild(..), defaultThingToBuild
  , RebuildUnconditionally(..)
  , toShakeOptions
  , rules
  , run
  )
where

import Control.Exception
import Data.Binary
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromJust)
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
  { optTemplates :: [ThingToBuild Maybe [Either FilePath FilePattern]]
  , optOutputExtension :: String
  , optOutputDirectory :: FilePath
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

instance Semigroup RebuildUnconditionally where
  SomeThings a <> SomeThings b = SomeThings (a <> b)
  _ <> Everything = Everything
  Everything <> _ = Everything

data WithProvenance a
  = FromCommandLine a
  | FromConfigFile FilePath a
  deriving ( Functor, Eq, Show )

instance Foldable WithProvenance where
  foldMap f (FromCommandLine a) = f a
  foldMap f (FromConfigFile _ a) = f a

unwrapProvenance :: WithProvenance a -> a
unwrapProvenance = \case
  (FromCommandLine a) -> a
  (FromConfigFile _ a) -> a

instance Semigroup (WithProvenance a) where
  FromCommandLine _ <> b@(FromCommandLine _) = b
  a@(FromCommandLine _) <> FromConfigFile _ _ = a
  FromConfigFile _ _ <> b@(FromCommandLine _) = b
  FromConfigFile _ _ <> b@(FromConfigFile _ _) = b

data WithPlicity a
  = With Plicity (WithProvenance a)
  deriving ( Functor, Eq, Show )

instance Foldable WithPlicity where
  foldMap f (With _ prov) = foldMap f prov

unwrapPlicity :: WithPlicity a -> a
unwrapPlicity (With _ prov) = unwrapProvenance prov

data Plicity
  = Explicit | Implicit
  deriving ( Eq, Show )

instance Semigroup (WithPlicity a) where
  With Explicit a <> With Explicit b = With Explicit (a <> b)
  With Explicit a <> With Implicit _b = With Explicit a
  With Implicit _a <> With Explicit b = With Explicit b
  a@(With Implicit (FromConfigFile _ _)) <> With Implicit (FromCommandLine _) = a
  With Implicit (FromCommandLine _) <> b@(With Implicit (FromConfigFile _ _)) = b
  With Implicit a <> With Implicit b = With Implicit (a <> b)

-- | A 'ThingToBuild' is a description of how to build a particular set of
-- templates. The first type parameter is a functor – in practise, it is
-- instanstiated to either 'Maybe' or 'Identity'. With 'Maybe', some fields can
-- be left unspecified, to be filled in with defaults later; with defaults
-- filled in, @f@ changes to 'Identity'. The second type parameter is intended
-- to be filled with a specification of what templates to build – e.g., a
-- 'FilePath', a 'FilePattern', a list of either of those.
data ThingToBuild f a = ThingToBuild
  { buildWhat :: a
  , buildOutputExtension :: f String
  , buildOutputDirectory :: f FilePath
  , buildDelimiters :: f Delimiters }
  deriving ( Functor )

deriving instance ((forall b. Show b => Show (f b)), Show a) => Show (ThingToBuild f a)

-- | For documentation purposes.
type SourcePath = FilePath
-- | For documentation purposes.
type TargetPath = FilePath

instance Foldable (ThingToBuild f) where
  foldMap f ThingToBuild{buildWhat} = f buildWhat

instance Traversable (ThingToBuild f) where
  traverse f ThingToBuild{buildWhat, ..} =
    (\a -> ThingToBuild{buildWhat = a, ..}) <$> f buildWhat

defaultThingToBuild :: a -> ThingToBuild Maybe a
defaultThingToBuild a =
  ThingToBuild a Nothing Nothing Nothing

expand :: ThingToBuild f [Either SourcePath FilePattern] -> IO (ThingToBuild f [SourcePath])
expand =
    traverse expandPatterns . fmap partitionEithers
  where
    expandPatterns (paths, patterns) =
      (paths ++) <$> getDirectoryFilesIO "" patterns

specify :: Options -> ThingToBuild Maybe a -> ThingToBuild Identity a
specify Options{..} ThingToBuild{..} =
  ThingToBuild
    { buildWhat
    , buildOutputExtension = Identity $
        optOutputExtension `fromMaybe` buildOutputExtension
    , buildOutputDirectory = Identity $
        -- N.B. We /prepend/ the top-level output dir!
        optOutputDirectory </> ("." `fromMaybe` buildOutputDirectory)
    , buildDelimiters = Identity $
        optDelimiters `fromMaybe` buildDelimiters }

deriveTargetPath :: ThingToBuild Identity SourcePath -> TargetPath
deriveTargetPath ThingToBuild{..} =
  normalise $ runIdentity buildOutputDirectory </> buildWhat `replaceExtensions` runIdentity buildOutputExtension

createTargetToSourceMap :: [ThingToBuild Identity [SourcePath]] -> HashMap TargetPath (ThingToBuild Identity SourcePath)
createTargetToSourceMap thingsToBuild =
    Map.fromList $ concatMap explode thingsToBuild
  where
    explode :: ThingToBuild Identity [SourcePath] -> [(TargetPath, ThingToBuild Identity SourcePath)]
    explode =
      map (\thing -> (deriveTargetPath thing, thing)) . sequence

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

fileRules :: HashMap TargetPath (ThingToBuild Identity SourcePath) -> Rules ()
fileRules targetToSourceMap = do
    (`Map.member` targetToSourceMap) ?> \targetPath -> do
      let thingToBuild = fromJust $ Map.lookup targetPath targetToSourceMap
          templatePath = buildWhat thingToBuild
          delimiters = runIdentity (buildDelimiters thingToBuild)
      val <- lookupField (TemplateFile delimiters Bindings.empty) templatePath specialBodyField
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
      let record' = Map.insert specialBodyField (Output markdownOutput) record
      return $ MarkdownValue $ Record record'

    readTemplate (templatePath, delimiters) = do
      need [templatePath]
      pathAbs <- liftIO $ System.makeAbsolute templatePath
      putInfo $ "Reading template from " <> pathAbs
      input <- liftIO $ Text.readFile templatePath
      parsed <- eitherThrow $
        first (ParseError . errorBundlePretty) $
        parseTemplate delimiters templatePath input
      return $ ExecTemplate $ \templateParameters -> do
        (Bindings bindings, output) <- (>>= eitherThrow) $
          first (EvalError . toList) <$>
          runStmtT (for_ parsed evalStatement) templateParameters
        let record = Map.insert specialBodyField (Output (Output.toStorable output)) bindings
        return $ Record record

{-# INLINE builtinRules #-}

-- | The field we put the output of a template or the body of a markdown file
-- in.
specialBodyField :: Text
specialBodyField = "body"

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

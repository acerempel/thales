module DependencyMonad
  ( DependencyMonad(..)
  , FileType(..)
  , Options(..)
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
import Development.Shake.Rule
import qualified System.Directory as System
import Text.Megaparsec
import Text.MMark as MMark

import Parse
import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType -> FilePath -> Text -> m (Maybe Value)

instance DependencyMonad Action where

  listDirectory = getDirectoryContents

  lookupField ft path key = apply1 (FieldAccessQ ft path key)
  {-# INLINE lookupField #-}

-- | The command-line options.
data Options = Options
  { optTemplateFileExtension :: String
  , optOutputFileExtension :: String
  , optInputDirectory :: FilePath
  , optOutputDirectory :: FilePath
  , optRebuildUnconditionally :: Maybe RebuildUnconditionally
  , optDelimiters :: Delimiters
  , optTimings :: Bool }

data RebuildUnconditionally
  = SomeThings (NonEmpty FilePattern)
  | Everything

run :: Options -> Rules () -> IO ()
run options rules =
  shake (optionsToShakeOptions options) $ do
    readYamlCached <- newCache readYaml
    readMarkdownCached <- newCache readMarkdown
    addBuiltinRule noLint noIdentity (fieldAccessRuleRun readYamlCached readMarkdownCached)
    rules

  where
    optionsToShakeOptions Options{..} =
      shakeOptions
        { shakeRebuild = translateRebuild optRebuildUnconditionally
        , shakeTimings = optTimings }

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
      let record' = Map.insert "body" (Markdown mmark) record
      return $ MarkdownValue $ Record record'

{-# INLINEABLE run #-}

data FieldAccessQ = FieldAccessQ
  { faFileType :: FileType
  , faFilePath :: FilePath
    -- ^ The path to a file containing key-value pairs. This is assumed to be an
    -- absolute path.
  , faField :: Text
  } deriving stock ( Generic, Show, Eq, Typeable )
    deriving anyclass ( Hashable, Binary, NFData )

type instance RuleResult FieldAccessQ = Maybe Value

-- | A type of file that may be interpreted as a key-value mapping, i.e. a
-- 'Record'.
data FileType
  -- | The YAML file is assumed to have an associative array at the top level
  -- with string keys.
  = YamlFile
  -- | Any YAML front matter is treated as with 'YamlFile', and the document
  -- body is available under the "body" key.
  | MarkdownFile
  deriving stock ( Eq, Show, Generic )
  deriving anyclass ( Hashable, NFData, Typeable, Binary )

-- | This newtype exists solely to help me keep the order of arguments in
-- 'fieldAccessRuleRun' straight.
newtype YamlValue = YamlValue { fromYaml :: Value }
-- | See 'YamlValue'.
newtype MarkdownValue = MarkdownValue { fromMarkdown :: Value }

fieldAccessRuleRun ::
  (FilePath -> Action YamlValue) ->
  (FilePath -> Action MarkdownValue) ->
  FieldAccessQ ->
  Maybe ByteString ->
  RunMode ->
  Action (RunResult (Maybe Value))
fieldAccessRuleRun getYaml getMarkdown FieldAccessQ{..} mb_stored mode = do
  mb_record <-
    case faFileType of
      YamlFile -> fromYaml <$> getYaml faFilePath
      MarkdownFile -> fromMarkdown <$> getMarkdown faFilePath
  val <-
    case mb_record of
      Record hashMap ->
        return $ Map.lookup faField hashMap
      _ ->
        liftIO $ throwIO $ NotAnObject faFileType faFilePath
  case mb_stored of
    Nothing ->
      return $ RunResult
        ChangedRecomputeDiff
        (toStrict (encode (hash val)))
        val
    Just stored ->
      let
        previous_hash = decode (toLazy stored)
        (new_hash, did_change) =
          case mode of
            RunDependenciesSame ->
              (previous_hash, ChangedNothing)
            RunDependenciesChanged ->
              let changed =
                    if new_hash == previous_hash
                      then ChangedRecomputeSame else ChangedRecomputeDiff
              in (hash val, changed)
      in return $ RunResult
        did_change
        (toStrict (encode new_hash))
        val

data NotAnObject = NotAnObject FileType FilePath
  deriving stock ( Show, Typeable )

instance Exception NotAnObject

newtype MMarkException = MMarkException (ParseErrorBundle Text MMarkErr)
  deriving stock ( Show, Typeable )

instance Exception MMarkException where
  displayException (MMarkException err) =
    errorBundlePretty err

instance DependencyMonad IO where
  listDirectory dir = sort <$> System.listDirectory dir
  lookupField _ft _fp _t = fail "zrop"

hash = hashWithSalt defaultSalt

defaultSalt = -2578643520546668380

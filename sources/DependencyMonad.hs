module DependencyMonad
  ( DependencyMonad(..)
  , Options(..)
  , run
  )
where

import Control.Exception
import Data.Binary
import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Yaml as Yaml
import Development.Shake
import Development.Shake.Rule
import qualified System.Directory as System

import Parse
import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupYaml :: FilePath -> Text -> m (Maybe Value)

  -- lookupMarkdown :: FilePath -> Text -> m Value

instance DependencyMonad Action where

  listDirectory = getDirectoryContents

  lookupYaml path key = apply1 (YamlQ path key)

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
    readValueFromYamlFile <- newCache $ \path -> do
      need [path]
      Yaml.decodeFileThrow path
    addBuiltinRule noLint noIdentity (yamlRun readValueFromYamlFile)
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

data YamlQ = YamlQ
  { yamlPath :: FilePath
    -- ^ The path to a YAML file. This is assumed to be an absolute path.
  , yamlKey :: Text
  } deriving stock ( Generic, Show, Eq, Typeable )
    deriving anyclass ( Hashable, Binary, NFData )

type instance RuleResult YamlQ = Maybe Value

yamlRun ::
  (FilePath -> Action Value) ->
  YamlQ ->
  Maybe ByteString ->
  RunMode ->
  Action (RunResult (Maybe Value))
yamlRun readValueFromYamlFile YamlQ{..} mb_stored mode = do
  mb_record <- readValueFromYamlFile yamlPath
  val <-
    case mb_record of
      Record hashMap ->
        return $ Map.lookup yamlKey hashMap
      _ ->
        liftIO $ throwIO $ NotAnObject YamlQ{..}
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

newtype NotAnObject = NotAnObject YamlQ
  deriving ( Show, Typeable )

instance Exception NotAnObject

instance DependencyMonad IO where
  listDirectory dir = sort <$> System.listDirectory dir
  lookupYaml _fp _t = fail "zrop"
  -- lookupMarkdown _fp _t = fail "zmop"

hash = hashWithSalt defaultSalt

defaultSalt = -2578643520546668380

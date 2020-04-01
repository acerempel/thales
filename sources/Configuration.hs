module Configuration
  ( Options(..)
  , Verbosity(..)
  , ThingToBuild(..), defaultThingToBuild
  , RebuildUnconditionally(..)
  , toShakeOptions
  , specify, expand
  , createTargetToSourceMap
  , SourcePath, TargetPath
  )
where

import qualified Data.HashMap.Strict as Map
import qualified Data.List.NonEmpty as NonEmpty
import Development.Shake
import Development.Shake.FilePath

import Syntax

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


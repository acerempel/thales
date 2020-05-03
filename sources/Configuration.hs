module Configuration
  ( Options(..)
  , Verbosity(..)
  , ThingToBuild(..)
  , defaultTemplatePattern
  , RebuildUnconditionally(..)
  , toShakeOptions
  , specify
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
  { optInputDirectory :: FilePath
  , optTemplatePattern :: FilePattern
  , optOutputDirectory :: FilePath
  , optRebuildUnconditionally :: Maybe RebuildUnconditionally
  , optBaseTemplate :: Maybe FilePath
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

data RebuildUnconditionally
  = SomeThings (NonEmpty FilePattern)
  | Everything
  deriving stock ( Show, Eq )

instance Semigroup RebuildUnconditionally where
  SomeThings a <> SomeThings b = SomeThings (a <> b)
  _ <> Everything = Everything
  Everything <> _ = Everything

-- | A 'ThingToBuild' is a description of how to build a particular set of
-- templates. The first type parameter is a functor – in practise, it is
-- instanstiated to either 'Maybe' or 'Identity'. With 'Maybe', some fields can
-- be left unspecified, to be filled in with defaults later; with defaults
-- filled in, @f@ changes to 'Identity'. The second type parameter is intended
-- to be filled with a specification of what templates to build – e.g., a
-- 'FilePath', a 'FilePattern', a list of either of those.
data ThingToBuild = ThingToBuild
  { buildSourceFile :: SourcePath
  -- ^ N.B. this is /relative/ to the source directory! Might want to encode
  -- that in the type at some point.
  -- | Head applied last, i.e. outermost
  , buildBaseTemplates :: [SourcePath] }
  deriving ( Show )

-- | For documentation purposes.
type SourcePath = FilePath
-- | For documentation purposes.
type TargetPath = FilePath

defaultTemplatePattern :: FilePattern
defaultTemplatePattern =
  "**" </> "*"

specify :: Options -> IO (HashMap TargetPath ThingToBuild)
specify Options{..} = do
  sourceFilesUnfiltered <- getDirectoryFilesIO optInputDirectory [optTemplatePattern]
  let sourceFiles = filter (not . isHiddenFileName . takeFileName) sourceFilesUnfiltered
  return $ Map.fromList $ map createThingToBuildPair sourceFiles
 where
  createThingToBuildPair sourceFile =
    (deriveTargetPath sourceFile, createThingToBuild sourceFile)
  createThingToBuild sourceFile =
    ThingToBuild
      { buildSourceFile = sourceFile
      , buildBaseTemplates = maybe [] one optBaseTemplate }
  isHiddenFileName fn =
    case fn of
      hd : _ | hd == '.' || hd == '_' -> True
      _ -> False
  deriveTargetPath sourceFile =
    optOutputDirectory </>
      if "html" `isExtensionOf` sourceFile && takeFileName sourceFile /= "index.html"
         then dropExtension sourceFile </> "index.html"
         else sourceFile

{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main (main) where

import Development.Shake (need, withoutActions, getDirectoryFilesIO)
import Development.Shake.Database
import Development.Shake.FilePath
import System.IO.Temp
import Test.Tasty
import Test.Tasty.Golden

import DependencyMonad
import Parse

main = do
  goldenFiles <- getDirectoryFilesIO "" [dir </> "*" <.> goldenExtension]
  let tests = map createGoldenTest goldenFiles
  withSystemTempDirectory "thales-golden-tests" $ \tempOutputDir -> do
    let options = createOptions tempOutputDir
    shakeWithDatabase (toShakeOptions options) (withoutActions (rules options)) $ \db ->
      defaultMain $ testGroup "Golden tests" (traverse runShakeTest tests $ (db, tempOutputDir))
 where
  dir = "tests/golden/files"
  templateExtension = "tpl"
  goldenExtension = "out"
  createOptions outputDirectory =
    Options
      { optTemplates = [Right (dir </> "*" <.> templateExtension)]
      , optOutputExtension = goldenExtension
      , optOutputDirectory = outputDirectory
      , optRebuildUnconditionally = Nothing
      , optDelimiters = Delimiters "{" "}"
      , optVerbosity = Info
      , optTimings = False
      , optCacheDirectory = "/dev/null" }

newtype ShakeTest = ShakeTest { runShakeTest :: (ShakeDatabase, FilePath) -> TestTree }

createGoldenTest goldenFile =
  ShakeTest $ \(database, outputDir) -> do
    let outputFile = outputDir </> goldenFile
    goldenVsFile testName goldenFile outputFile (runTemplate database outputFile)
  where
    testName = takeFileName goldenFile

runTemplate database outputPath = do
  ([()], []) <- shakeRunDatabase database [need [outputPath]]
  return ()

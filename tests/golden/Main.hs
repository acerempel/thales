{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
module Main where

import Development.Shake (need, withoutActions)
import Development.Shake.Database
import Development.Shake.FilePath
import System.Directory
import Test.Tasty
import Test.Tasty.Golden

import DependencyMonad
import Parse

main = do
  templateFiles <- findByExtension [".tpl"] dir
  mb_tests <- runMaybeT $ traverse (liftA2 (<|>) goldenTestFile goldenTestDir) templateFiles
  let tests =  fromMaybe (error "No output files found!") mb_tests
  shakeWithDatabase (toShakeOptions options) (withoutActions (rules options)) $ \db ->
    defaultMain $ testGroup "Golden tests" (traverse runShakeTest tests $ db)
 where
  dir = "tests/golden/files"
  options =
    Options
      { optTemplates = [Right (dir </> "*" <.> "tpl")]
      , optOutputExtension = "out"
      , optRebuildUnconditionally = Nothing
      , optDelimiters = Delimiters "{" "}"
      , optVerbosity = Info
      , optTimings = False
      , optCacheDirectory = "/dev/null" }

goldenTestFile templateFile = do
  let goldenFile = templateFile -<.> "out"
  goldenExists <- liftIO $ doesFileExist goldenFile
  MaybeT $ return $
    if goldenExists
      then let testName = takeBaseName templateFile
           in Just $ createGoldenTest testName goldenFile
      else Nothing

goldenTestDir templateFile = do
  let dir = dropExtension templateFile
  dirExists <- liftIO $ doesDirectoryExist dir
  MaybeT $
    if dirExists
      then do
        goldenFiles <- findByExtension [".out"] dir
        let tests = traverse (runShakeTest . createTest) goldenFiles
            groupName = takeBaseName dir
        return (Just (ShakeTest (\db -> testGroup groupName (tests db))))
      else return Nothing
  where
    createTest goldenFile =
      let testName = takeBaseName goldenFile
      in createGoldenTest testName goldenFile

newtype ShakeTest = ShakeTest { runShakeTest :: ShakeDatabase -> TestTree }

createGoldenTest testName goldenFile =
  ShakeTest $ \database ->
    goldenVsFile testName goldenFile outputFile (runTemplate database outputFile)
 where
  outputFile = dropExtension goldenFile

runTemplate database outputPath = do
  ([()], []) <- shakeRunDatabase database [need [outputPath]]
  return ()

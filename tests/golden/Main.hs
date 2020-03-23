{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
module Main where

import Control.Exception
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.IO as Text
import qualified Data.Yaml as Yaml
import System.FilePath
import System.Directory
import Test.Tasty
import Test.Tasty.Golden
import Text.Megaparsec

import Eval
import Bindings
import qualified Output
import Parse
import Value

main = do
  let dir = "tests/golden/files"
  templateFiles <- findByExtension [".tpl"] dir
  mb_tests <- runMaybeT $ traverse (liftA2 (<|>) goldenTestFile goldenTestDir) templateFiles
  let tests =  fromMaybe (error "No output files found!") mb_tests
  let goldenTests = testGroup "Golden tests" tests
  defaultMain goldenTests

goldenTestFile templateFile = do
  let outputPath = templateFile -<.> "out"
  outExists <- liftIO $ doesFileExist outputPath
  MaybeT $ return $
    if outExists
      then let testName = takeBaseName templateFile
               bindingsFile = templateFile -<.> "yaml"
           in Just $ createGoldenTest testName templateFile outputPath bindingsFile
      else Nothing

goldenTestDir templateFile = do
  let dir = dropExtension templateFile
  dirExists <- liftIO $ doesDirectoryExist dir
  MaybeT $
    if dirExists
      then do
        outFiles <- findByExtension [".out"] dir
        let tests = map createTest outFiles
            groupName = takeBaseName dir
        return (Just (testGroup groupName tests))
      else return Nothing
  where
    createTest outFile =
      let testName = takeBaseName outFile
          bindingsFile = outFile -<.> "yaml"
      in createGoldenTest testName templateFile outFile bindingsFile


createGoldenTest testName templateFile outputFile bindingsFile =
  goldenVsString testName outputFile (runTemplate templateFile bindingsFile)

runTemplate tplPath yamlPath = do
  input  <- Text.readFile tplPath
  let delimiters = Delimiters "{" "}"
  parsed <- either throwIO pure $
            first (ParseError . errorBundlePretty) $
            parseTemplate delimiters tplPath input
  yamlExists <- doesFileExist yamlPath
  bindings <-
    if yamlExists
      then Yaml.decodeFileThrow yamlPath
      else pure mempty
  eval'd <- (>>= either throwIO pure) $
            first (EvalError . toList) . second snd <$>
            runStmtT (for_ parsed evalStatement) bindings
  return $ Builder.toLazyByteString $ Output.toBuilder eval'd

instance Yaml.FromJSON Bindings where
  parseJSON =
    Yaml.withObject "Bindings"
    (fmap Bindings . traverse Yaml.parseJSON)

newtype TplParseError =
  ParseError String
  deriving newtype Show

instance Exception TplParseError where
  displayException (ParseError err) = err

newtype EvalError = EvalError [Problem]
  deriving stock Show

instance Exception EvalError

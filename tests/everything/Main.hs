{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Control.Exception
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding as Text
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Text.Megaparsec

import Eval
import Parse

main = do
  templateFiles <- findByExtension [".tpl"] "tests/everything/golden"
  let goldenTests =
        testGroup "Golden tests" (fmap createGoldenTest templateFiles)
  defaultMain goldenTests

createGoldenTest tplFile =
  goldenVsString
    (takeBaseName tplFile)
    (replaceExtension tplFile ".html")
    (runTemplate tplFile)

runTemplate tplPath = do
  input  <- Text.readFile tplPath
  parsed <- either throwIO pure $
            first (ParseError . errorBundlePretty) $
            parseTemplate defaultDelimiters tplPath input
  eval'd <- (>>= either throwIO pure) $
            first (EvalError . toList) . second snd <$>
            runStmtM (for_ parsed evalStatement) mempty
  return $ Text.encodeUtf8 $ Builder.toLazyText eval'd

newtype TplParseError =
  ParseError String
  deriving newtype Show

instance Exception TplParseError where
  displayException (ParseError err) = err

newtype EvalError = EvalError [Problem]
  deriving stock Show

instance Exception EvalError

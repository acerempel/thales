{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main where

import Control.Exception
import qualified Data.Yaml as Yaml
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding as Text
import System.FilePath
import System.Directory
import Test.Tasty
import Test.Tasty.Golden
import Text.Megaparsec
import Unsafe.Coerce (unsafeCoerce)

import Eval
import Bindings
import Parse
import Syntax
import Value

main = do
  templateFiles <- findByExtension [".tpl"] "tests/everything/golden"
  let goldenTests =
        testGroup "Golden tests" (fmap createGoldenTest templateFiles)
  defaultMain goldenTests

createGoldenTest tplFile =
  goldenVsString testName outputFile (runTemplate tplFile bindingsFile)
  where
    bindingsFile = replaceExtension tplFile ".yaml"
    outputFile = replaceExtension tplFile ".html"
    testName = takeBaseName tplFile

runTemplate tplPath yamlPath = do
  input  <- Text.readFile tplPath
  parsed <- either throwIO pure $
            first (ParseError . errorBundlePretty) $
            parseTemplate defaultDelimiters tplPath input
  yamlExists <- doesFileExist yamlPath
  bindings <-
    if yamlExists
      then Yaml.decodeFileThrow yamlPath
      else pure mempty
  eval'd <- (>>= either throwIO pure) $
            first (EvalError . toList) . second snd <$>
            runStmtM (for_ parsed evalStatement) bindings
  return $ Text.encodeUtf8 $ Builder.toLazyText eval'd

instance Yaml.FromJSON Bindings where
  parseJSON =
    Yaml.withObject "Bindings"
    (fmap Bindings . traverse yamlValueToValue . coerceHashMap)

-- | b/c type role HashMap nominal representational
coerceHashMap :: HashMap Text a -> HashMap Name a
coerceHashMap = unsafeCoerce

yamlValueToValue :: Yaml.Value -> Yaml.Parser Value
yamlValueToValue val =
  case val of
    Yaml.Object obj ->
      Record . coerceHashMap <$> (traverse yamlValueToValue obj)
    Yaml.Array arr ->
      Array <$> (traverse yamlValueToValue arr)
    Yaml.String str ->
      pure $ String str
    Yaml.Number num ->
      pure $ Number num
    Yaml.Bool boo ->
      pure $ Boolean boo
    Yaml.Null ->
      fail "Null not supported!"

newtype TplParseError =
  ParseError String
  deriving newtype Show

instance Exception TplParseError where
  displayException (ParseError err) = err

newtype EvalError = EvalError [Problem]
  deriving stock Show

instance Exception EvalError

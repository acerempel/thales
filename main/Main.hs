{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main (main) where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import Control.Exception
import Development.Shake
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative
import Options.Applicative.Help.Pretty
import System.Directory
import System.FilePath
import System.IO hiding (print)
import Text.Megaparsec (errorBundlePretty)

import qualified Bindings
import DependencyMonad hiding (listDirectory)
import Eval
import qualified NonEmptyText
import qualified Output
import Parse

main = do
  options@Options{..} <- execParser cli
  when (optVerbosity >= Verbose) $
    print options
  run options $ do
    want optTargets
    dependDelimiters <- addOracle pure
    (`elem` optTargets) ?> \targetPath -> do
      let templatePath = targetPath <.> templateExtension
      need [templatePath]
      _ <- dependDelimiters optDelimiters
      execTemplate optDelimiters templatePath targetPath

execTemplate :: Delimiters -> FilePath -> FilePath -> Action ()
execTemplate delimiters templatePath targetPath = do
  input <- liftIO $ Text.readFile templatePath
  parsed <-
    either (liftIO . throwIO) pure $
    first (ParseError . errorBundlePretty) $
    parseTemplate delimiters templatePath input
  eval'd <-
    (>>= either (liftIO . throwIO) pure) $
      first (EvalError . toList) <$>
      runStmtT (for_ parsed evalStatement) Bindings.empty
  liftIO $ withBinaryFile targetPath WriteMode $ \hdl -> do
    hSetBuffering hdl (BlockBuffering Nothing)
    Output.write hdl (snd eval'd)

type instance RuleResult Delimiters = Delimiters

newtype TemplateParseError =
  ParseError String
  deriving newtype Show

instance Exception TemplateParseError where
  displayException (ParseError err) = err

newtype TemplateEvalError = EvalError [Problem]
  deriving stock Show

instance Exception TemplateEvalError

cli =
  info (optionsParser <**> helper) $
    fullDesc <>
    progDesc "A nice templating engine." <>
    headerDoc (Just
      (vsep
        [ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
        , "--- The Thales templating system ---"
        , "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" ]
      `flatAlt`
      "~~~ The Thales templating system ~~~"))

optionsParser =
  Options
    <$> targetArguments
    <*> rebuildOption
    <*> delimitersOption
    <*> verbosityOption
    <*> timingsOption
  where
    targetArguments =
      some $ strArgument $
        metavar "TARGET" <>
        completer (listIOCompleter getPossibleTargets)
    rebuildOption =
      (Just . SomeThings <$> NE.some (strOption $
        metavar "PATTERN" <>
        long "rebuild" <>
        short 'r' <>
        help "Rebuild targets matching this glob pattern")) <|>
      (flag Nothing (Just Everything) (
        long "rebuild-all" <> short 'R' <>
        help "Rebuild everything"))
    delimitersOption =
      option (eitherReader parseDelimiters) $
        long "delimiters" <>
        short 'd' <>
        metavar "DELIMITERS" <>
        value defaultDelimiters <>
        showDefaultWith showDelimiters <>
        help "Delimiters for template directives"
    verbosityOption =
      flag' Silent (long "silent" <> short 's') <|>
      flag' Warn (long "warn" <> short 'w') <|>
      flag' Info (long "info") <|>
      flag' Verbose (long "verbose" <> short 'v') <|>
      flag' Diagnostic (long "diagnostic") <|>
      pure Warn
    timingsOption =
      switch $
        long "timings" <>
        short 't' <>
        help "Print timings"

getPossibleTargets :: IO [String]
getPossibleTargets =
  map dropExtension . filter (templateExtension `isExtensionOf`) <$> listDirectory "."

templateExtension = "template"

parseDelimiters :: String -> Either String Delimiters
parseDelimiters s = maybe (Left errMsg) Right $ do
  let (before, comma_after) = break (== delimitersSeparator) s
  after <- viaNonEmpty tail comma_after
  before_ne <- nonEmpty before
  after_ne <- nonEmpty after
  return $
    Delimiters
      (NonEmptyText.fromNonEmptyString before_ne)
      (NonEmptyText.fromNonEmptyString after_ne)
 where
  errMsg =
    "Argument is not of the form \"<string>,<string>\""

showDelimiters :: Delimiters -> String
showDelimiters Delimiters{..} =
  Text.unpack (NonEmptyText.toText begin) <>
  [delimitersSeparator] <>
  Text.unpack (NonEmptyText.toText end)

delimitersSeparator = ','

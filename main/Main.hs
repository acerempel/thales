{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Main (main) where

import qualified Control.Applicative.Combinators.NonEmpty as NE
import Development.Shake
import qualified Data.Text as Text
import Options.Applicative
import Options.Applicative.Help.Pretty
import System.Directory

import Configuration
import DependencyMonad hiding (listDirectory)
import Syntax

main =
  run =<< customExecParser cliPrefs cli

cli =
  info (optionsParser <**> helper) $
    fullDesc <>
    progDesc cliDescription <>
    headerDoc (Just cliHeader) <>
    footer cliFooter

cliPrefs =
  prefs $ showHelpOnEmpty <> columns 72

cliHeader =
  vsep
    [ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
    , "––– The Thales templating system –––"
    , "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" ]
  `flatAlt`
  "~~~ The Thales templating system ~~~"

cliFooter =
  "Thales takes template files, containing text of whatever shape you like interspersed " <>
  "with specially delimited template directives, and interprets the directives, " <>
  "substituting their results back into the template to produce the target file. " <>
  "Template directives allow for inclusion of values from YAML files, Markdown files, " <>
  "and from other templates. Thales tracks the dependencies of each template, so that if " <>
  "a target is requested which has been built before and whose dependencies have not " <>
  "since changed, it will not be rebuilt."

cliDescription =
  "Thales is a simple textual templating system with dependency tracking. " <>
  "For further documentation, see the README on GitHub at https://github.com/parsonyorick/thales-templating#readme."

optionsParser =
  Options
    <$> templatesOptions
    <*> outputExtensionOption
    <*> outputDirectoryOption
    <*> rebuildOption
    <*> baseTemplateOption
    <*> delimitersOption
    <*> verbosityOption
    <*> timingsOption
    <*> ephemeralOption
  where
    templatesOptions =
      fmap ((: []) . defaultThingToBuild) . some $
        (fmap Left . strOption $
          metavar "FILE" <>
          long "template-file" <>
          short 't' <>
          completer (listIOCompleter (listDirectory ".")) <>
          help ("A template file to be compiled. This option may be given multiple times to " <>
                "compile multiple files.")) <|>
        (fmap Right . strOption $
          metavar "PATTERN" <>
          long "template-pattern" <>
          short 'p' <>
          completer (listIOCompleter (listDirectory ".")) <>
          help ("A glob pattern that should match the templates to be compiled. This option " <>
                "may be given multiple times; all template files matching any of the patterns " <>
                "will be built."))
    outputExtensionOption =
      strOption $
        metavar "EXTENSION" <>
        long "output-extension" <>
        long "out-ext" <>
        help ("The file extension for output files. The filename of an output file is constructed from " <>
              "the filename of the corresponding template file by replacing all file extensions with " <>
              "the output file extension.")
    outputDirectoryOption =
      strOption $
        metavar "DIRECTORY" <>
        long "output-directory" <>
        long "out-dir" <>
        value "." <>
        showDefaultWith (const "current directory") <>
        help ("The directory in which to place output files.")
    rebuildOption =
      (Just . SomeThings <$> NE.some (strOption $
        metavar "PATTERN" <>
        long "rebuild" <>
        short 'r' <>
        help ("Rebuild targets matching this glob pattern, regardless of whether their " <>
              "dependencies have changed. This option may be given multiple times to specify multiple patterns."))) <|>
      (flag Nothing (Just Everything) (
        long "rebuild-all" <> short 'R' <>
        help "Rebuild all targets, regardless of whether dependencies have changed."))
    baseTemplateOption =
      option (Just <$> str) $
        metavar "FILE" <>
        long "base-template" <>
        short 'b' <>
        help ("Use this file as the top-level base template. The templates to be compiled will be passed" <>
              "to the base template under the name 'content' (as the result of calling `load-template(<FILE>)`.)") <>
        value Nothing <>
        showDefaultWith (const "none")
    delimitersOption =
      option (eitherReader parseDelimiters) $
        long "delimiters" <>
        short 'd' <>
        metavar "BEGIN,END" <>
        value defaultDelimiters <>
        showDefaultWith showDelimiters <>
        help "A pair of strings, separated by a comma, that delimit template directives."
    verbosityOption =
      flag' Silent (long "silent" <> short 's' <> help "Print no messages.") <|>
      flag' Warn (long "warn" <> short 'w' <> help "Print errors and warnings only.") <|>
      flag' Info (
        long "info" <> short 'i' <>
        help ("Print brief messages telling you what Thales is up to, plus errors and warnings. " <>
              "(This is the default.)")) <|>
      flag' Verbose (
        long "verbose" <> short 'v' <>
        help ("Print message for essentially everything that happens, " <>
              "plus everything that --info prints.")) <|>
      flag' Diagnostic (
        long "diagnostic" <>
        hidden <>
        help ("Print all manner of debugging info, " <>
              "plus everything that --verbose prints.")) <|>
      pure Info
    timingsOption =
      switch $
        long "timings" <>
        short 't' <>
        hidden <>
        help "Print some coarse-grained timing information that Shake calculates."
    ephemeralOption =
      flag ".thales" "/dev/null" $
        long "ephemeral" <>
        hidden <>
        help "Do not use the build system metadata cache."

parseDelimiters :: String -> Either String Delimiters
parseDelimiters s = maybe (Left errMsg) Right $ do
  let (before, comma_after) = break (== delimitersSeparator) s
  after <- viaNonEmpty tail comma_after
  return $ Delimiters (Text.pack before) (Text.pack after)
 where
  errMsg =
    "Argument is not of the form \"<string>,<string>\""

showDelimiters :: Delimiters -> String
showDelimiters Delimiters{..} =
  Text.unpack begin <>
  [delimitersSeparator] <>
  Text.unpack end

delimitersSeparator = ','

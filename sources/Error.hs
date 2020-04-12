module Error
  ( ThalesException(..)
  , displayShakeException
  )
where

import Data.Text.Prettyprint.Doc
import Data.Yaml (YamlException)
import Development.Shake
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Text.MMark (MMarkErr)

import Eval.Problem
import Value

data ThalesException
  = TemplateEvalError [StmtProblem]
  | TemplateParseError String
  | MarkdownParseError (ParseErrorBundle Text MMarkErr)
  | NotAnObject FileType FilePath
  | MarkdownYamlParseError FilePath YamlException
  deriving Show

instance Exception ThalesException

displayShakeException :: ShakeException -> Doc Markup
displayShakeException ShakeException{..}
  | Just thalesException <- fromException shakeExceptionInner =
    displayThalesException thalesException
  | otherwise =
    vsep (map fineagle shakeExceptionStack) <> line <>
    pretty (displayException shakeExceptionInner) <> line
  where
    fineagle str =
      case str of
        '*':' ':rest -> pretty '•' <+> pretty rest
        ' ':' ':rest -> indent 2 (pretty rest)
        _ ->            pretty str

displayThalesException :: ThalesException -> Doc Markup
displayThalesException = \case
  TemplateEvalError problems ->
    vsep (punctuate line (map displayStmtProblem problems))
  TemplateParseError str ->
    pretty str <> line
  NotAnObject _ft fp ->
    "not an object: " <> pretty fp -- TODO
  MarkdownParseError peb -> pretty (errorBundlePretty peb)
  MarkdownYamlParseError fp _err -> pretty fp <> ": YAML error!"

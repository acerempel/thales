module Error
  ( ThalesException(..)
  , displayShakeException
  )
where

import Data.Text.Prettyprint.Doc
import Development.Shake

import Eval.Problem
import Value

data ThalesException
  = TemplateEvalError [StmtProblem]
  | TemplateParseError String
  | NotAnObject FileType FilePath
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

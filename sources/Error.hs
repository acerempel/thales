module Error where

import Text.Megaparsec (ParseErrorBundle)
import Text.MMark (MMarkErr)

import Eval.Problem
import Value

data ThalesException
  = TemplateEvalError [Problem]
  | TemplateParseError String
  | MarkdownParseError (ParseErrorBundle Text MMarkErr)
  | NotAnObject FileType FilePath



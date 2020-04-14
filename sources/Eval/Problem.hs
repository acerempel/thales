module Eval.Problem
  ( ExprProblemInContext(..)
  , ExprProblem(..), StmtProblem(..)
  , FunctionCallProblem(..), FieldAccessProblem(..)
  , RecordBindingProblem(..)
  , ForProblem(..), IncludeBodyProblem(..)
  , TypeMismatch(..), ArgumentTypeMismatches(..)
  , WrongNumberOfArguments(..), InsufficientArguments(..)
  , AddProblemContext
  , Markup(..), markupToAnsi
  , displayStmtProblem
  )
where

import Data.DList (DList)
import qualified Data.IntMap.Strict as IntMap
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import Text.Megaparsec (unPos)

import Syntax
import Value

data Markup = Problematic | Heading

markupToAnsi :: Markup -> AnsiStyle
markupToAnsi = \case
  Problematic -> color Magenta <> bold
  Heading -> bold

data TypeMismatch = TypeMismatch Value (DList SomeValueType)
  deriving ( Show, Eq )

-- | Note that this instance assumes the 'Value' is the same!
instance Semigroup TypeMismatch where
  (TypeMismatch val types1) <> (TypeMismatch _ types2) =
    TypeMismatch val (types1 <> types2)

newtype ArgumentTypeMismatches = ArgumentTypeMismatches
  { fromArgumentTypeMismatches :: IntMap TypeMismatch }
  deriving ( Show, Eq )

instance Semigroup ArgumentTypeMismatches where
  a1 <> a2 = ArgumentTypeMismatches $
    IntMap.unionWith (<>) (fromArgumentTypeMismatches a1) (fromArgumentTypeMismatches a2)

instance Monoid ArgumentTypeMismatches where
  mempty = ArgumentTypeMismatches IntMap.empty

type AddProblemContext =
  ExprProblemInContext -> ExprF ExprProblemInContext

data ExprProblemInContext
  = Here ExprProblem
  | OK Expr
  | Within (ExprF ExprProblemInContext)
  deriving Show

displayExprProblemInContext :: ExprProblemInContext -> Doc Markup
displayExprProblemInContext = \case
  Here prob -> displayExprProblem prob
  OK expr -> displayExpr expr
  Within expf -> displayExprF displayExprProblemInContext expf

data StmtProblem
  = ExprProblem SourcePos ExprProblemInContext
  | ExprIsNotOutputable SourcePos Expr Value
  | ForProblem SourcePos ForProblem
  | IncludeBodyProblem SourcePos IncludeBodyProblem
  | OptionallyExprProblem SourcePos (Maybe Name) ExprProblemInContext
  | ExportProblem SourcePos [Either (RecordBinding Expr) RecordBindingProblem]
  | LetProblem SourcePos [(Name, ExprProblemInContext)]
  deriving Show

data ForProblem
  = ForNotAnArray Name Expr Value
  | ForExprProblem Name ExprProblemInContext
  deriving Show

data IncludeBodyProblem
  = IncludeBodyNotADocument Expr Value
  | IncludeBodyExprProblem ExprProblemInContext
  deriving Show

displayStmtProblem :: StmtProblem -> Doc Markup
displayStmtProblem = \case
  ExprProblem sp epic ->
    displayError sp $ displayExprProblemInContext epic
  _ -> error "nyi!"

data ExprProblem
  = FunctionCallProblem Name [Expr] FunctionCallProblem
  | FieldAccessProblem Name Expr FieldAccessProblem
  | RecordLiteralProblem [Either (RecordBinding Expr) RecordBindingProblem]
  | NameNotFound Name [Name]
  deriving Show

data FunctionCallProblem
  = FunctionDoesNotExist [Name]
  | FunctionArgumentTypeMismatches ArgumentTypeMismatches
  | FunctionInsufficientArguments InsufficientArguments
  | FunctionWrongNumberOfArguments WrongNumberOfArguments
  deriving Show

data FieldAccessProblem
  = FieldAccessFieldNotFound [Name]
  | FieldAccessNotARecord Value
  deriving Show

data RecordBindingProblem
  = RecordBindingNameNotFound Name
  deriving Show

data WrongNumberOfArguments
  = WrongNumberOfArguments { expected :: Int, actual :: Int }
  deriving Show

newtype InsufficientArguments
  = InsufficientArguments Int
  deriving Show

displayExprProblem :: ExprProblem -> Doc Markup
displayExprProblem = \case
  NameNotFound name available ->
    let nnfMessage =
          "Name not found!" <> availMessage
        availMessage =
          if null available
             then softline'
             else line <> nest 2 ("These names are available:" <> line <>
              fillSep (punctuate comma (map displayName available))) <> softline
     in annotate Problematic (displayName name) <+>
          annotate Heading (pretty '←' <+> align nnfMessage)
  _ -> error "nyi!"

displayError :: SourcePos -> Doc any -> Doc any
displayError SourcePos{..} doc =
  pretty sourceName <> ":" <>
  pretty (unPos sourceLine) <> ":" <>
  pretty (unPos sourceColumn) <>
  line <> indent 2 doc

module Eval.Problem
  ( ExprProblemInContext(..)
  , ExprProblem(..), StmtProblem(..)
  , FunctionCallProblem(..), FieldAccessProblem(..)
  , RecordBindingProblem(..)
  , ForProblem(..), IncludeBodyProblem(..)
  , TypeMismatch(..), ArgumentTypeMismatches(..)
  , WrongNumberOfArguments(..), InsufficientArguments(..)
  , AddProblemContext
  , Markup(..), markupToAnsi -- TODO move to Problem
  )
where

import Data.DList (DList)
import qualified Data.IntMap.Strict as IntMap
import Data.Text.Prettyprint.Doc.Render.Terminal

import List (List)
import Syntax
import Value

data Markup = Problematic | Heading

markupToAnsi :: Markup -> AnsiStyle
markupToAnsi = \case
  Problematic -> color Magenta <> bold
  Heading -> bold

data TypeMismatch = TypeMismatch Value (DList SomeValueType)
  deriving ( Eq )

-- | Note that this instance assumes the 'Value' is the same!
instance Semigroup TypeMismatch where
  (TypeMismatch val types1) <> (TypeMismatch _ types2) =
    TypeMismatch val (types1 <> types2)

newtype ArgumentTypeMismatches = ArgumentTypeMismatches
  { fromArgumentTypeMismatches :: IntMap TypeMismatch }
  deriving ( Eq )

instance Semigroup ArgumentTypeMismatches where
  a1 <> a2 = ArgumentTypeMismatches $
    IntMap.unionWith (<>) (fromArgumentTypeMismatches a1) (fromArgumentTypeMismatches a2)

instance Monoid ArgumentTypeMismatches where
  mempty = ArgumentTypeMismatches IntMap.empty

type AddProblemContext =
  ExprProblemInContext -> ExprProblemInContext

data ExprProblemInContext
  = Here ExprProblem
  | OK Expr
  | Within (ExprF ExprProblemInContext)

data StmtProblem
  = ExprProblem SourcePos ExprProblemInContext
  | ForProblem SourcePos ForProblem
  | IncludeBodyProblem SourcePos IncludeBodyProblem
  | OptionallyExprProblem SourcePos (Maybe Name) ExprProblemInContext
  | ExportProblem SourcePos [Either (RecordBinding Expr) RecordBindingProblem]
  | LetProblem SourcePos [(Name, ExprProblemInContext)]

data ForProblem
  = ForNotAnArray Name Expr Value
  | ForExprProblem Name ExprProblemInContext

data IncludeBodyProblem
  = IncludeBodyNotADocument Expr Value
  | IncludeBodyExprProblem ExprProblemInContext

data ExprProblem
  = FunctionCallProblem Name (List Expr) FunctionCallProblem
  | FieldAccessProblem Name Expr FieldAccessProblem
  | RecordLiteralProblem [Either (RecordBinding Expr) RecordBindingProblem]
  | NameNotFound Name

data FunctionCallProblem
  = FunctionDoesNotExist
  | FunctionArgumentTypeMismatches ArgumentTypeMismatches
  | FunctionInsufficientArguments InsufficientArguments
  | FunctionWrongNumberOfArguments WrongNumberOfArguments

data FieldAccessProblem
  = FieldAccessFieldNotFound [Name]
  | FieldAccessNotARecord Value

data RecordBindingProblem
  = RecordBindingNameNotFound Name

data WrongNumberOfArguments
  = WrongNumberOfArguments { expected :: Int, actual :: Int }
  deriving ( Eq, Show )

newtype InsufficientArguments
  = InsufficientArguments Int
  deriving ( Eq, Show )

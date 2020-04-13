module Eval.Problem
  ( Problem(..), ProblemWhere(..), ProblemDescription(..)
  , TypeMismatch(..), ArgumentTypeMismatches(..)
  , WrongNumberOfArguments(..), InsufficientArguments(..)
  , problemAddContext, problemSetSource, AddProblemContext
  )
where

import Data.DList (DList)
import qualified Data.IntMap.Strict as IntMap

import Syntax
import Value

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

data Problem = Problem
  { problemWhere :: ProblemWhere (ExprH ProblemWhere)
  , problemDescription :: ProblemDescription }
  deriving ( Eq )

type AddProblemContext =
  ProblemWhere (ExprH ProblemWhere) -> ExprH ProblemWhere

problemAddContext :: AddProblemContext -> Problem -> Problem
problemAddContext add Problem{..} =
  Problem{problemWhere = ProblemWithin (add problemWhere), ..}

problemSetSource :: Expr -> Problem -> Problem
problemSetSource expr Problem{..} =
  Problem{problemWhere =
    case problemWhere of
      Nowhere -> ProblemHere expr
      _ -> problemWhere
  , .. }

data ProblemWhere a
  = ProblemHere Expr
  | ProblemWithin a
  | NoProblem Expr
  | Nowhere
  deriving ( Show, Eq )

data ProblemDescription
  = ProblemTypeMismatch TypeMismatch
  | ProblemWrongNumberOfArguments WrongNumberOfArguments
  | ProblemInsufficientArguments InsufficientArguments
  | ProblemArgumentTypeMismatches ArgumentTypeMismatches
  | ProblemNameNotFound Name [Name]
  | ProblemUnknownFunction Name [Name]
  | ProblemFieldNotFound Name [Name]
  deriving ( Eq )

data WrongNumberOfArguments
  = WrongNumberOfArguments { expected :: Int, actual :: Int }
  deriving ( Eq, Show )

newtype InsufficientArguments
  = InsufficientArguments Int
  deriving ( Eq, Show )

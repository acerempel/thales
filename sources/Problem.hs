module Problem
  ( Problem(..), ProblemWhere(..), ProblemDescription(..)
  , TypeMismatch(..), ArgumentErrors(..)
  , WrongNumberOfArguments(..)
  , problemAddContext, problemSetSource, AddProblemContext
  )
where

import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.IntMap.Strict as IntMap

import Syntax
import Value

data TypeMismatch = TypeMismatch Value (DList ValueType)
  deriving ( Show, Eq )

-- | Note that this instance assumes the 'Value' is the same!
instance Semigroup TypeMismatch where
  (TypeMismatch val types1) <> (TypeMismatch _ types2) =
    TypeMismatch val (types1 <> types2)

newtype ArgumentErrors = ArgumentErrors
  { fromArgumentErrors :: IntMap TypeMismatch }
  deriving ( Show, Eq )

instance Semigroup ArgumentErrors where
  a1 <> a2 = ArgumentErrors $
    IntMap.unionWith (<>) (fromArgumentErrors a1) (fromArgumentErrors a2)

instance Monoid ArgumentErrors where
  mempty = ArgumentErrors IntMap.empty

data Problem = Problem
  { problemWhere :: ProblemWhere (ExprH ProblemWhere)
  , problemDescription :: ProblemDescription }
  deriving ( Show, Eq )

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
  | ProblemArgumentErrors ArgumentErrors
  | ProblemNameNotFound Name
  | ProblemUnknownFunction Name
  | ProblemFieldNotFound Name [Name]
  deriving ( Show, Eq )

data WrongNumberOfArguments
  = WrongNumberOfArguments { expected :: Int, actual :: Int }
  deriving ( Eq, Show )

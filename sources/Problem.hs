module Problem
  ( Problem(..), ProblemWhere(..), ProblemDescription(..)
  , TypeMismatch(..), ArgumentErrors(..)
  )
where

import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.IntMap.Strict as IntMap

import Syntax
import Value

data TypeMismatch = TypeMismatch Value (DList ValueType)

-- | Note that this instance assumes the 'Value' is the same!
instance Semigroup TypeMismatch where
  (TypeMismatch val types1) <> (TypeMismatch _ types2) =
    TypeMismatch val (types1 <> types2)

newtype ArgumentErrors = ArgumentErrors
  { fromArgumentErrors :: IntMap TypeMismatch }

instance Semigroup ArgumentErrors where
  a1 <> a2 = ArgumentErrors $
    IntMap.unionWith (<>) (fromArgumentErrors a1) (fromArgumentErrors a2)

instance Monoid ArgumentErrors where
  mempty = ArgumentErrors IntMap.empty

data Problem = Problem

data ProblemWhere = ProblemWhere

data ProblemDescription
  = ProblemTypeMismatch TypeMismatch
  | ProblemWrongNumberOfArguments Name Int Int
  | ProblemArgumentErrors ArgumentErrors
  | ProblemNameNotFound Name
  | ProblemUnknownFunction Name
  | ProblemFieldNotFound Name [Name]

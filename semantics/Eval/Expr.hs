{-# LANGUAGE StrictData #-}
module Eval.Expr
  ( EvalM, Bindings, runEvalM
  , Problem(..), ProblemWhere(..)
  , ProblemDescription(..), AddProblemContext
  , zutAlors, handleZut, mapZut, addProblemSource
  , lookup, localBindings, liftEval
  )
where

import Control.Monad.Trans.Except
import Data.Functor.Classes
import qualified Data.HashMap.Strict as Map
import Text.Show

import BaseMonad
import Syntax
import Value

-- | A 'Bindings' provides the bindings that are available at the top level
-- in a template. At the moment it is just a type synonym, but this will
-- probably change.
type Bindings = HashMap Name Value

newtype EvalM a = EvalM
  { unEvalM :: ExceptT Problem (ReaderT Bindings M) a }
  deriving ( Monad, Functor, Applicative )

runEvalM :: EvalM a -> Bindings -> M (Either Problem a)
runEvalM (EvalM m) bindings =
  runReaderT (runExceptT m) bindings

-- | Abort this evaluation with the given problem.
zutAlors :: ProblemDescription -> EvalM a
zutAlors prob = EvalM (throwE (Problem Nowhere prob))

lookup :: Name -> EvalM (Maybe Value)
lookup n = EvalM (asks (Map.lookup n))

localBindings :: (Bindings -> Bindings) -> EvalM a -> EvalM a
localBindings f (EvalM r) = EvalM (local f r)

handleZut :: (Problem -> EvalM a) -> EvalM a -> EvalM a
handleZut handler (EvalM m) = EvalM (catchE m (unEvalM . handler))

mapZut :: AddProblemContext -> EvalM a -> EvalM a
mapZut f (EvalM m) =
  EvalM $ withExceptT (problemAddContext f) m

addProblemSource :: Expr -> EvalM a -> EvalM a
addProblemSource source (EvalM m) =
  EvalM $ withExceptT (problemSetSource source) m

liftEval :: M a -> EvalM a
liftEval m = EvalM (lift (lift m))

data ProblemDescription
  = NameNotFound Name
  | FieldNotFound Name Value
  | NotARecord Value
  | NotAnArray Value
  deriving ( Show, Eq )

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

instance Show1 ProblemWhere where
  liftShowsPrec showsPrecA _showListA prec = \case
    ProblemWithin nested ->
      -- TODO: use prec correctly!
      ("ProblemWithin (" <>)
      . showsPrecA prec nested
      . (')' :)
    ProblemHere expr ->
      ("ProblemHere (" <>)
      . showsPrec prec expr
      . (')' :)
    NoProblem expr ->
      ("NoProblem (" <>)
      . showsPrec prec expr
      . (')' :)
    Nowhere ->
      ("Nowhere" <>)

instance Eq1 ProblemWhere where
  liftEq eqA p1 p2 =
    case (p1, p2) of
      (ProblemHere e1, ProblemHere e2) ->
        e1 == e2
      (NoProblem e1, NoProblem e2) ->
        e1 == e2
      (Nowhere, Nowhere) ->
        True
      (ProblemWithin a1, ProblemWithin a2) ->
        eqA a1 a2
      _ ->
        False


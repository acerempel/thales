module Eval.Expr
  ( ExprM, Bindings, runExprM, Name
  , Problem(..), ProblemWhere(..)
  , ProblemDescription(..), AddProblemContext
  , zutAlors, handleZut, mapZut, addProblemSource
  , lookup, addLocalBindings, liftEval
  )
where

import Control.Monad.Trans.Except
import Data.Functor.Classes
import qualified Data.HashMap.Strict as Map
import Text.Show

import BaseMonad
import Eval.Bindings
import Syntax
import Value

newtype ExprM a = ExprM
  { unExprM :: ExceptT Problem (ReaderT Bindings M) a }
  deriving ( Monad, Functor, Applicative )

runExprM :: ExprM a -> Bindings -> M (Either Problem a)
runExprM (ExprM m) bindings =
  runReaderT (runExceptT m) bindings

-- | Abort this evaluation with the given problem.
zutAlors :: ProblemDescription -> ExprM a
zutAlors prob = ExprM (throwE (Problem Nowhere prob))

lookup :: Name -> ExprM (Maybe Value)
lookup n = ExprM (asks (Map.lookup n))

instance HasLocalBindings ExprM where
  addLocalBindings binds (ExprM r) =
    -- 'binds' has to be the first argument to 'Map.union', so that we can shadow
    -- existing bindings -- 'Map.union' is left-biased.
    ExprM (local (Map.fromList binds `Map.union`) r)

handleZut :: (Problem -> ExprM a) -> ExprM a -> ExprM a
handleZut handler (ExprM m) = ExprM (catchE m (unExprM . handler))

mapZut :: AddProblemContext -> ExprM a -> ExprM a
mapZut f (ExprM m) =
  ExprM $ withExceptT (problemAddContext f) m

addProblemSource :: Expr -> ExprM a -> ExprM a
addProblemSource source (ExprM m) =
  ExprM $ withExceptT (problemSetSource source) m

liftEval :: M a -> ExprM a
liftEval m = ExprM (lift (lift m))

data ProblemDescription
  = NameNotFound Name
  | FieldNotFound Name Value
  | NotARecord Value
  | NotAnArray Value
  | NotText Value
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

module Eval.Expr
  ( ExprT, Bindings, runExprT, Name
  , Problem(..), ProblemWhere(..)
  , ProblemDescription(..), AddProblemContext
  , zutAlors, handleZut, mapZut, addProblemSource
  , lookupName, addLocalBindings
  )
where

import Prelude hiding (local, ask, asks)

import Control.Monad.Trans.Except
import Control.Applicative.Trans.Reader
import Data.Functor.Classes
import Text.Show

import Bindings
import Syntax
import Value

newtype ExprT m a = ExprT
  { unExprT :: ExceptT Problem (ReaderT Bindings m) a }
  deriving newtype ( Monad, Functor, Applicative )

instance MonadTrans ExprT where
  lift = ExprT . lift . lift

instance MonadIO m => MonadIO (ExprT m) where
  liftIO = lift . liftIO

runExprT :: ExprT m a -> Bindings -> m (Either Problem a)
runExprT (ExprT m) bindings =
  runReaderT (runExceptT m) bindings

-- | Abort this evaluation with the given problem.
zutAlors :: Monad m => ProblemDescription -> ExprT m a
zutAlors prob = ExprT (throwE (Problem Nowhere prob))

lookupName :: Monad m => Name -> ExprT m (Maybe Value)
lookupName n = ExprT $ lift $ asks (Bindings.lookup n)

instance Applicative m => HasLocalBindings (ExprT m) where
  addLocalBindings binds (ExprT r) =
    -- 'binds' has to be the first argument to 'Map.union', so that we can shadow
    -- existing bindings -- 'Map.union' is left-biased.
    ExprT (mapExceptT (local (Bindings.fromList binds `Bindings.union`)) r)

handleZut :: Monad m => (Problem -> ExprT m a) -> ExprT m a -> ExprT m a
handleZut handler (ExprT m) = ExprT (catchE m (unExprT . handler))

mapZut :: Functor m => AddProblemContext -> ExprT m a -> ExprT m a
mapZut f (ExprT m) =
  ExprT $ withExceptT (problemAddContext f) m

addProblemSource :: Functor m => Expr -> ExprT m a -> ExprT m a
addProblemSource source (ExprT m) =
  ExprT $ withExceptT (problemSetSource source) m

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


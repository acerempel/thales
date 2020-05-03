module Eval.Expr
  ( ExprT, Bindings, runExprT, Name
  , exprProblem, mapZut
  , lookupName
  , getLocalNames
  , HasLocalBindings(..)
  )
where

import Prelude hiding (local, ask, asks)

import Control.Monad.Trans.Except
import Control.Applicative.Trans.Reader
import qualified Data.HashMap.Strict as Map

import Bindings
import Eval.Problem
import Syntax
import Value

newtype ExprT m a = ExprT
  (ExceptT ExprProblemInContext (ReaderT Env m) a)
  deriving newtype ( Monad, Functor, Applicative )

-- | The environment for evaluation of a template expression.
newtype Env = Env
  { envLocalBindings :: Bindings
  -- ^ The bindings that are in scope when evaluating this expression.
  }

overBindings :: (Bindings -> Bindings) -> Env -> Env
overBindings f Env{envLocalBindings, ..} =
  Env{envLocalBindings = f envLocalBindings, ..}

instance MonadTrans ExprT where
  lift = ExprT . lift . lift

instance MonadIO m => MonadIO (ExprT m) where
  liftIO = lift . liftIO

runExprT :: ExprT m a -> Bindings -> m (Either ExprProblemInContext a)
runExprT (ExprT m) bindings =
  runReaderT (runExceptT m) (Env bindings)

-- | Abort this evaluation with the given problem.
exprProblem :: Monad m => ExprProblem -> ExprT m a
exprProblem prob = ExprT (throwE (Here prob))

lookupName :: Monad m => Name -> ExprT m (Maybe Value)
lookupName (Name n) = ExprT $ lift $ asks (Bindings.lookup n . envLocalBindings)

getLocalNames :: Monad m => ExprT m [Name]
getLocalNames =
  ExprT $ lift $ asks $
    coerce . Map.keys . envLocalBindings

instance Applicative m => HasLocalBindings (ExprT m) where
  addLocalBindings binds (ExprT r) =
    -- 'binds' has to be the first argument to 'Map.union', so that we can shadow
    -- existing bindings -- 'Map.union' is left-biased.
    ExprT (mapExceptT (local (overBindings (Bindings.fromList (coerce binds) `Bindings.union`))) r)

mapZut :: Functor m => AddProblemContext -> ExprT m a -> ExprT m a
mapZut f (ExprT m) =
  ExprT $ withExceptT (Within . f) m

class HasLocalBindings m where

  addLocalBindings :: [(Name, Value)] -> m a -> m a

  addLocalBinding :: Name -> Value -> m a -> m a
  addLocalBinding n v =
    addLocalBindings [(n, v)]

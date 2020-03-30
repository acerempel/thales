module Eval.Expr
  ( ExprT, Bindings, runExprT, Name
  , zutAlors, handleZut, mapZut, addProblemSource
  , typeMismatch
  , lookupName, addLocalBindings
  , getTemplateDirectory
  , getTemplateDelimiters
  )
where

import Prelude hiding (local, ask, asks)

import Control.Monad.Trans.Except
import Control.Applicative.Trans.Reader
import Data.DList (DList)

import Bindings
import Problem
import Syntax
import Value

newtype ExprT m a = ExprT
  { unExprT :: ExceptT Problem (ReaderT Env m) a }
  deriving newtype ( Monad, Functor, Applicative )

-- | The environment for evaluation of a template expression.
data Env = Env
  { envLocalBindings :: Bindings
  -- ^ The bindings that are in scope when evaluating this expression.
  , envTemplateDirectory :: FilePath
  -- ^ The directory containing the file from which we got the template we are
  -- evaluating.
  , envTemplateDelimiters :: Delimiters
  -- ^ The delimiters with which the template was parsed.
  }

overBindings :: (Bindings -> Bindings) -> Env -> Env
overBindings f Env{envLocalBindings, ..} =
  Env{envLocalBindings = f envLocalBindings, ..}

instance MonadTrans ExprT where
  lift = ExprT . lift . lift

instance MonadIO m => MonadIO (ExprT m) where
  liftIO = lift . liftIO

runExprT :: ExprT m a -> FilePath -> Delimiters -> Bindings -> m (Either Problem a)
runExprT (ExprT m) dir delims bindings =
  runReaderT (runExceptT m) (Env bindings dir delims)

-- | Abort this evaluation with the given problem.
zutAlors :: Monad m => ProblemDescription -> ExprT m a
zutAlors prob = ExprT (throwE (Problem Nowhere prob))

typeMismatch :: Monad m => Value -> DList ValueType -> ExprT m a
typeMismatch val types =
  zutAlors (ProblemTypeMismatch (TypeMismatch val types))

lookupName :: Monad m => Name -> ExprT m (Maybe Value)
lookupName n = ExprT $ lift $ asks (Bindings.lookup n . envLocalBindings)

-- | Get the directory of the file the template under evaluation came from.
getTemplateDirectory :: Monad m => ExprT m FilePath
getTemplateDirectory = ExprT $ lift $ asks envTemplateDirectory

getTemplateDelimiters :: Monad m => ExprT m Delimiters
getTemplateDelimiters = ExprT $ lift $ asks envTemplateDelimiters

instance Applicative m => HasLocalBindings (ExprT m) where
  addLocalBindings binds (ExprT r) =
    -- 'binds' has to be the first argument to 'Map.union', so that we can shadow
    -- existing bindings -- 'Map.union' is left-biased.
    ExprT (mapExceptT (local (overBindings (Bindings.fromList binds `Bindings.union`))) r)

handleZut :: Monad m => (Problem -> ExprT m a) -> ExprT m a -> ExprT m a
handleZut handler (ExprT m) = ExprT (catchE m (unExprT . handler))

mapZut :: Functor m => AddProblemContext -> ExprT m a -> ExprT m a
mapZut f (ExprT m) =
  ExprT $ withExceptT (problemAddContext f) m

addProblemSource :: Functor m => Expr -> ExprT m a -> ExprT m a
addProblemSource source (ExprT m) =
  ExprT $ withExceptT (problemSetSource source) m

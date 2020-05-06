module Eval.Statement
  ( StmtT, runStmtT
  , addOutput, addBinding, addBindings, addTopBindings
  , liftExprT, stmtProblem
  )
where

import Prelude hiding (foldr, local)

import Control.Applicative.Trans.Validation
import Control.Applicative.Trans.Writer
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as Map

import DependencyMonad.Class
import Eval.Expr
import Output
import Eval.Problem
import Syntax
import Value

newtype StmtT m a = StmtT
  { unStmtT :: ReaderT Env (WriterT ResultAccum (ValidationT (DList StmtProblem) m)) a }
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO)

-- | The environment for evaluation of a template statement.
newtype Env = Env
  { envLocalBindings :: IORef Bindings
  -- ^ The bindings that are in scope when evaluating this expression.
  }

instance MonadTrans StmtT where
  lift = StmtT . lift . lift . lift

instance Monad m => MonadReader (IORef (HashMap Text Value)) (StmtT m) where
  ask = StmtT (asks envLocalBindings)

data ResultAccum = Result
  { tplBindings :: Bindings
  , tplOutput :: DList Output }

instance Semigroup ResultAccum where
  Result tb1 to1 <> Result tb2 to2 =
    Result (tb1 `Map.union` tb2) (to1 <> to2)

instance Monoid ResultAccum where
  mempty = Result Map.empty mempty

runStmtT :: (Monad m, MonadIO m) => StmtT m () -> Bindings -> m (Either (DList StmtProblem) (Bindings, Output))
runStmtT stm bindings = do
  bindingsRef <- newIORef bindings
  let env = Env bindingsRef
      massage ((), Result { tplBindings, tplOutput }) =
        (tplBindings, DList.foldr (<>) mempty tplOutput)
  fmap (fmap massage) $ runValidationT (runWriterT (runReaderT (unStmtT stm) env))

addOutput :: Monad m => Output -> StmtT m ()
addOutput o =
  StmtT (lift (tell (Result Map.empty (DList.singleton o))))

instance (Monad m, MonadIO m) => HasLocalBindings (StmtT m) where

  addLocalBindings new_binds stmt = do
    let appendBindings binds =
          ( Map.fromList (coerce new_binds) `Map.union` binds, binds )
    bindingsRef <- ask
    old_binds <- atomicModifyIORef' bindingsRef appendBindings
    result <- stmt
    atomicWriteIORef bindingsRef old_binds
    return result

  addLocalBinding (Name n) v stmt = do
    bindingsRef <- ask
    old_binds <- atomicModifyIORef' bindingsRef (\m -> (Map.insert n v m, m))
    res <- stmt
    atomicWriteIORef bindingsRef old_binds
    return res

addTopBindings :: MonadIO m => [(Name, Value)] -> StmtT m ()
addTopBindings new_binds = do
  bindingsRef <- ask
  atomicModifyIORef' bindingsRef (\m -> (Map.fromList (coerce new_binds) `Map.union` m, ()))

addBinding :: Monad m => Name -> Value -> StmtT m ()
addBinding (Name n) v =
  StmtT (lift (tell (Result (Map.singleton n v) mempty)))

addBindings :: Monad m => [(Name, Value)] -> StmtT m ()
addBindings binds =
  StmtT (lift (tell (Result (Map.fromList (coerce binds)) mempty)))

stmtProblem :: Monad m => StmtProblem -> StmtT m a
stmtProblem prob =
  StmtT (lift (lift (failure (DList.singleton prob))))

liftExprT :: DependencyMonad m => (ExprProblemInContext -> StmtProblem) -> ExprT m a -> StmtT m a
liftExprT wrap expr = do
  bindingsRef <- ask
  bindings <- readIORef bindingsRef
  eitherResult <- lift $ runExprT expr bindings
  case eitherResult of
    Left err -> StmtT . lift . lift . failure $ DList.singleton (wrap err)
    Right res -> return res

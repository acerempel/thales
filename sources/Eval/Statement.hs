module Eval.Statement
  ( StmtT, runStmtT
  , addOutput, addBinding, addBindings
  , liftExprT
  )
where

import Prelude hiding (foldr, local)

import Control.Applicative.Trans.Reader
import Control.Applicative.Trans.Validation
import Control.Applicative.Trans.Writer
import Data.DList (DList)
import qualified Data.DList as DList

import BaseMonad
import Bindings
import Eval.Expr
import Output
import Value

newtype StmtT m a = StmtT
  { unStmtT :: ReaderT Bindings (WriterT ResultAccum (ValidationT (DList Problem) m)) a }
  deriving newtype (Functor, Applicative, Alternative, Monad)

data ResultAccum = Result
  { tplBindings :: Bindings
  , tplOutput :: DList Output }

instance Semigroup ResultAccum where
  Result tb1 to1 <> Result tb2 to2 =
    Result (tb1 `Bindings.union` tb2) (to1 <> to2)

instance Monoid ResultAccum where
  mempty = Result Bindings.empty mempty

runStmtT :: Monad m => StmtT m () -> Bindings -> m (Either (DList Problem) (Bindings, Output))
runStmtT stm b =
  let massage ((), Result { tplBindings, tplOutput }) =
        (tplBindings, DList.foldr (<>) mempty tplOutput)
  in fmap (fmap massage) $ runValidationT (runWriterT (runReaderT (unStmtT stm) b))

addOutput :: Monad m => Output -> StmtT m ()
addOutput o =
  StmtT (lift (tell (Result Bindings.empty (DList.singleton o))))

instance HasLocalBindings (StmtT m) where

  addLocalBindings binds (StmtT m) =
    StmtT (local (Bindings.fromList binds `Bindings.union`) m)

  addLocalBinding n v (StmtT m) =
    StmtT (local (Bindings.insert n v) m)

addBinding :: Monad m => Name -> Value -> StmtT m ()
addBinding n v =
  StmtT (lift (tell (Result (Bindings.singleton n v) mempty)))

addBindings :: Monad m => [(Name, Value)] -> StmtT m ()
addBindings binds =
  StmtT (lift (tell (Result (Bindings.fromList binds) mempty)))

liftExprT :: BaseMonad m => ExprT m a -> StmtT m a
liftExprT expr =
  StmtT $ ReaderT $ \bindings ->
    let mE = runExprT expr bindings
        mD = fmap
               ( second (\a -> (a, mempty))
               . first DList.singleton)
               mE
        vD = ValidationT mD
        wD = WriterT vD
    in wD

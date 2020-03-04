module Eval.Statement
  ( StmtM, runStmtM
  , addOutput, addBinding, addBindings
  , liftExprM
  )
where

import Prelude hiding (foldr)

import Control.Applicative.Writer
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as Map
import Data.Text.Lazy.Builder
import Data.Validation as Validation

import BaseMonad
import Eval.Expr
import Value

newtype StmtM a = StmtM
  { unStmtM :: Bindings -> M (WriterT ResultAccum (Validation (DList Problem)) a) }

data ResultAccum = Result
  { tplBindings :: Bindings
  , tplOutput :: DList Builder }

instance Semigroup ResultAccum where
  Result tb1 to1 <> Result tb2 to2 =
    Result (tb1 `Map.union` tb2) (to1 <> to2)

instance Monoid ResultAccum where
  mempty = Result Map.empty mempty

instance Functor StmtM where
  fmap f (StmtM m) =
    StmtM $ \b -> fmap (fmap f) (m b)

instance Applicative StmtM where
  pure a = StmtM (\_ -> pure (pure a))
  (StmtM mf) <*> (StmtM ma) =
    StmtM $ \b -> liftA2 (<*>) (mf b) (ma b)

runStmtM :: StmtM a -> Bindings -> M (Validation (DList Problem) (a, Bindings, Builder))
runStmtM (StmtM m) b = do
  w <- m b
  let massage (a, Result { tplBindings, tplOutput }) =
        (a, tplBindings, DList.foldr (<>) mempty tplOutput)
  return (fmap massage (runWriterT w))

addOutput :: Builder -> StmtM ()
addOutput o =
  StmtM (\_ -> pure (tell (Result Map.empty (DList.singleton o))))

addBinding :: Name -> Value -> StmtM ()
addBinding n v =
  StmtM (\_ -> pure (tell (Result (Map.singleton n v) mempty)))

addBindings :: [(Name, Value)] -> StmtM ()
addBindings binds =
  StmtM (\_ -> pure (tell (Result (Map.fromList binds) mempty)))

liftExprM :: ExprM a -> StmtM a
liftExprM expr =
  StmtM $ \bindings ->
    WriterT
    . second (\a -> (a, mempty))
    . first DList.singleton
    . Validation.fromEither
    <$> runExprM expr bindings

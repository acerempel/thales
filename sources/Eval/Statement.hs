module Eval.Statement
  ( StmtT, runStmtT
  , addOutput, addBinding, addBindings
  , liftExprT
  )
where

import Prelude hiding (foldr)

import Control.Applicative.Trans.Writer
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Validation as Validation

import BaseMonad
import Bindings
import Eval.Expr
import Output
import Value

newtype StmtT m a = StmtT
  { unStmtT :: Bindings -> m (WriterT ResultAccum (Validation (DList Problem)) a) }

data ResultAccum = Result
  { tplBindings :: Bindings
  , tplOutput :: DList Output }

instance Semigroup ResultAccum where
  Result tb1 to1 <> Result tb2 to2 =
    Result (tb1 `Bindings.union` tb2) (to1 <> to2)

instance Monoid ResultAccum where
  mempty = Result Bindings.empty mempty

instance Functor m => Functor (StmtT m) where
  fmap f (StmtT m) =
    StmtT $ \b -> fmap (fmap f) (m b)

instance Applicative m => Applicative (StmtT m) where
  pure a = StmtT (\_ -> pure (pure a))
  (StmtT mf) <*> (StmtT ma) =
    StmtT $ \b -> liftA2 (<*>) (mf b) (ma b)

instance Monad m => Monad (StmtT m) where
  (StmtT m) >>= f =
    StmtT $ \binds ->
      m binds >>= \w ->
        case runWriterT w of
          Validation.Failure problems ->
            pure $ WriterT (Validation.Failure problems)
          Validation.Success (a, accum) ->
            case f a of
              StmtT m' ->
                m' binds >>= \w' -> pure $ WriterT $
                  case runWriterT w' of
                    Validation.Failure problems' ->
                      Validation.Failure problems'
                    Validation.Success (b, accum') ->
                      Validation.Success (b, accum <> accum')

runStmtT :: Monad m => StmtT m () -> Bindings -> m (Either (DList Problem) (Bindings, Output))
runStmtT (StmtT m) b = do
  w <- m b
  let massage ((), Result { tplBindings, tplOutput }) =
        (tplBindings, DList.foldr (<>) mempty tplOutput)
  return (Validation.toEither $ fmap massage (runWriterT w))

addOutput :: Applicative m => Output -> StmtT m ()
addOutput o =
  StmtT (\_ -> pure (tell (Result Bindings.empty (DList.singleton o))))

instance HasLocalBindings (StmtT m) where

  addLocalBindings binds (StmtT m) =
    StmtT (m . (Bindings.fromList binds `Bindings.union`))

  addLocalBinding n v (StmtT m) =
    StmtT (m . Bindings.insert n v)

addBinding :: Applicative m => Name -> Value -> StmtT m ()
addBinding n v =
  StmtT (\_ -> pure (tell (Result (Bindings.singleton n v) mempty)))

addBindings :: Applicative m => [(Name, Value)] -> StmtT m ()
addBindings binds =
  StmtT (\_ -> pure (tell (Result (Bindings.fromList binds) mempty)))

liftExprT :: BaseMonad m => ExprT m a -> StmtT m a
liftExprT expr =
  StmtT $ \bindings ->
    WriterT
    . second (\a -> (a, mempty))
    . first DList.singleton
    . Validation.fromEither
    <$> runExprT expr bindings

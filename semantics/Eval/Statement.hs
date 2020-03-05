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
import Eval.Bindings
import Eval.Expr
import Value
import Verbatim

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

instance Monad StmtM where
  (StmtM m) >>= f =
    StmtM $ \binds ->
      m binds >>= \w ->
        case runWriterT w of
          Validation.Failure problems ->
            pure $ WriterT (Validation.Failure problems)
          Validation.Success (a, accum) ->
            case f a of
              StmtM m' ->
                m' binds >>= \w' -> pure $ WriterT $
                  case runWriterT w' of
                    Validation.Failure problems' ->
                      Validation.Failure problems'
                    Validation.Success (b, accum') ->
                      Validation.Success (b, accum <> accum')

runStmtM :: StmtM a -> Bindings -> M (Validation (DList Problem) (a, Bindings, Builder))
runStmtM (StmtM m) b = do
  w <- m b
  let massage (a, Result { tplBindings, tplOutput }) =
        (a, tplBindings, DList.foldr (<>) mempty tplOutput)
  return (fmap massage (runWriterT w))

addOutput :: Verbatim -> StmtM ()
addOutput o =
  StmtM (\_ -> pure (tell (Result Map.empty (DList.singleton (fromVerbatim o)))))

instance HasLocalBindings StmtM where

  addLocalBindings binds (StmtM m) =
    StmtM (m . (Map.fromList binds `Map.union`))

  addLocalBinding n v (StmtM m) =
    StmtM (m . Map.insert n v)

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

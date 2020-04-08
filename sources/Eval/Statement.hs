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
import Development.Shake.FilePath

import {-# SOURCE #-} DependencyMonad
import Bindings
import Eval.Expr
import Output
import Problem
import Syntax
import Value

newtype StmtT m a = StmtT
  { unStmtT :: ReaderT Env (WriterT ResultAccum (ValidationT (DList Problem) m)) a }
  deriving newtype (Functor, Applicative, Alternative, Monad)

-- | The environment for evaluation of a template statement.
data Env = Env
  { envLocalBindings :: Bindings
  -- ^ The bindings that are in scope when evaluating this expression.
  , envTemplatePath :: FilePath
  -- ^ The file from which we got the template we are evaluating.
  , envTemplateDelimiters :: Delimiters
  -- ^ The delimiters with which the template was parsed.
  }

overBindings :: (Bindings -> Bindings) -> Env -> Env
overBindings f Env{envLocalBindings, ..} =
  Env{envLocalBindings = f envLocalBindings, ..}

data ResultAccum = Result
  { tplBindings :: Bindings
  , tplOutput :: DList Output }

instance Semigroup ResultAccum where
  Result tb1 to1 <> Result tb2 to2 =
    Result (tb1 `Bindings.union` tb2) (to1 <> to2)

instance Monoid ResultAccum where
  mempty = Result Bindings.empty mempty

runStmtT :: Monad m => StmtT m () -> FilePath -> Delimiters -> Bindings -> m (Either (DList Problem) (Bindings, Output))
runStmtT stm path delimiters bindings =
  let env = Env bindings path delimiters
      massage ((), Result { tplBindings, tplOutput }) =
        (tplBindings, DList.foldr (<>) mempty tplOutput)
  in fmap (fmap massage) $ runValidationT (runWriterT (runReaderT (unStmtT stm) env))

addOutput :: Monad m => Output -> StmtT m ()
addOutput o =
  StmtT (lift (tell (Result Bindings.empty (DList.singleton o))))

instance HasLocalBindings (StmtT m) where

  addLocalBindings binds (StmtT m) =
    StmtT (local (overBindings (Bindings.fromList (coerce binds) `Bindings.union`)) m)

  addLocalBinding (Name n) v (StmtT m) =
    StmtT (local (overBindings (Bindings.insert n v)) m)

addBinding :: Monad m => Name -> Value -> StmtT m ()
addBinding (Name n) v =
  StmtT (lift (tell (Result (Bindings.singleton n v) mempty)))

addBindings :: Monad m => [(Name, Value)] -> StmtT m ()
addBindings binds =
  StmtT (lift (tell (Result (Bindings.fromList (coerce binds)) mempty)))

liftExprT :: DependencyMonad m => ExprT m a -> StmtT m a
liftExprT expr =
  StmtT $ ReaderT $ \env ->
    let mE = runExprT expr
              (takeDirectory (envTemplatePath env))
              (envTemplateDelimiters env)
              (envLocalBindings env)
        mD = fmap
               ( second (,mempty)
               . first DList.singleton)
               mE
        vD = ValidationT mD
        wD = WriterT vD
    in wD

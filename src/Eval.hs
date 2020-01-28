module Eval where

import Control.Monad.Trans.Except
import Data.Functor.Foldable
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as Vec

import Syntax
import Value

type Context m = HashMap Name (Value m)

newtype EvalT m a = EvalT
  { unEvalT :: ExceptT (Problematic m) (ReaderT (Context m) m) a }
  deriving ( Monad, Functor, Applicative )

runEvalT :: EvalT m a -> Context m -> m (Either (Problematic m) a)
runEvalT (EvalT m) = runReaderT (runExceptT m)

zutAlors :: Monad m => Problem m -> EvalT m a
zutAlors prob = EvalT (throwE (ProblemHere prob))

mapZut :: Monad m => (forall a. a -> ExprF (Either a Expr)) -> EvalT m b -> EvalT m b
mapZut f (EvalT m) = EvalT $ withExceptT (ProblemWithin . f) m

lookup :: Monad m => Name -> EvalT m (Maybe (Value m))
lookup n = EvalT (asks (Map.lookup n))

getContext :: Monad m => EvalT m (Context m)
getContext = EvalT ask

localContext :: Monad m => (Context m -> Context m) -> EvalT m a -> EvalT m a
localContext f (EvalT r) = EvalT (local f r)

liftEval :: Monad m => m a -> EvalT m a
liftEval m = (EvalT (lift (lift m)))

evalExpr :: Monad m => Expr -> EvalT m (Value m)
evalExpr (Fix expr) = case expr of

  NameE name -> do
    mVal <- lookup name
    case mVal of
      Just val -> return val
      Nothing -> zutAlors (NameNotFound name)

  FieldAccessE name subExpr -> do
    subVal <- mapZut (FieldAccessE name . Left) $ evalExpr subExpr
    case subVal of
      Record rec ->
        let ohNo = zutAlors (FieldNotFound name subVal subExpr)
        in maybe ohNo return (Map.lookup name rec)
      _ ->
        zutAlors (NotARecord subVal subExpr)

  LiteralE lit ->
    return (literalToValue lit)

  ArrayE arr -> do
    -- TODO: mapZut!
    vals <- traverse evalExpr arr
    return (Array (Vec.fromList vals))

  ApplyE funcExpr argExpr -> do
    func <- mapZut (\z -> ApplyE (Left z) (Right argExpr)) $ evalExpr funcExpr
    case func of
      Function typ f -> do
        arg <- mapZut (\z -> ApplyE (Right funcExpr) (Left z)) $ evalExpr argExpr
        case (typ, arg) of
          (NumberT,  Number  n) -> liftEval (f n)
          (StringT,  String  s) -> liftEval (f s)
          (BooleanT, Boolean b) -> liftEval (f b)
          (ArrayT,   Array   a) -> liftEval (f a)
          (RecordT,  Record  r) -> liftEval (f r)
          _ ->
            -- Application of higher-order functions is not yet supported!
            zutAlors (TypeMismatch (SomeValueType typ) funcExpr arg argExpr)
      _ ->
        zutAlors (NotAFunction func funcExpr argExpr)

data Problematic f
  = ProblemHere (Problem f)
  | ProblemWithin (ExprF (Either (Problematic f) Expr))
  deriving Show

data Problem f
  = NameNotFound Name
  | FieldNotFound Name (Value f) Expr
  | NotARecord (Value f) Expr
  | NotAFunction (Value f) Expr Expr
  | TypeMismatch (SomeValueType f) Expr (Value f) Expr
  deriving Show

{- TODO: how to write this? We will have to do the same thing with Statement 
that we did with Expr, namely, make it a functor, so that it may contain
either a source Expr or an (Either Problematic Value). -}
evalStatement :: Monad m => Statement -> EvalT m (Value m)
evalStatement _ = return undefined

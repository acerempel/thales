{-# LANGUAGE NoMonoLocalBinds #-}
module Eval where

import Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as Vec

import Syntax
import Value

-- | A 'Bindings' provides the bindings that are available at the top level
-- in a template. At the moment it is just a type synonym, but this will
-- probably change.
type Bindings m = HashMap Name (Value m)

newtype EvalT m a = EvalT
  { unEvalT :: ExceptT (Problematic m (ExprH (Problematic m))) (ReaderT (Bindings m) m) a }
  deriving ( Monad, Functor, Applicative )

runEvalT :: EvalT m a -> Bindings m -> m (Either (Problematic m (ExprH (Problematic m))) a)
runEvalT (EvalT m) = runReaderT (runExceptT m)

-- | Abort this evaluation with the given problem.
zutAlors :: Monad m => Problem m -> EvalT m a
zutAlors prob = EvalT (throwE (ProblemHere prob))

-- | If the second argument aborts with a problem, wrap the problem
-- with the first argument, which should represent
-- the expression that the caller of 'mapZut' is evaluating.
mapZut :: Monad m => AddProblemContext m -> EvalT m b -> EvalT m b
mapZut f (EvalT m) =
  EvalT $ withExceptT (ProblemWithin . f) m

lookup :: Monad m => Name -> EvalT m (Maybe (Value m))
lookup n = EvalT (asks (Map.lookup n))

getBindings :: Monad m => EvalT m (Bindings m)
getBindings = EvalT ask

localBindings :: Monad m => (Bindings m -> Bindings m) -> EvalT m a -> EvalT m a
localBindings f (EvalT r) = EvalT (local f r)

liftEval :: Monad m => m a -> EvalT m a
liftEval m = EvalT (lift (lift m))

type AddProblemContext m =
  Problematic m (ExprH (Problematic m)) -> ExprH (Problematic m)

evalSubExpr :: Monad m => AddProblemContext m -> Expr -> EvalT m (Value m)
evalSubExpr f = evalExpr (Just f)

evalTopExpr :: Monad m => Expr -> EvalT m (Value m)
evalTopExpr = evalExpr Nothing

evalExpr :: Monad m => Maybe (AddProblemContext m) -> Expr -> EvalT m (Value m)
evalExpr mContext expr =
 maybe id mapZut mContext $ case expr of

  NameE name -> do
    mVal <- lookup name
    case mVal of
      Just val -> return val
      Nothing -> zutAlors (NameNotFound name)

  FieldAccessE name (Id subExpr) -> do
    subVal <- evalSubExpr (FieldAccessE name) subExpr
    case subVal of
      Record rec ->
        let ohNo = zutAlors (FieldNotFound name subVal subExpr)
        in maybe ohNo return (Map.lookup name rec)
      _ ->
        zutAlors (NotARecord subVal subExpr)

  LiteralE lit ->
    return (literalToValue lit)

  ArrayE arr -> do
    let {- NoMonoLocalBinds above is needed so because addArrayProblemContext needs
           to be polymorphic in 'zut', because of the type of 'evalSubExpr'.
           MonoLocalBinds is implied by GADTs (from default-extensions). Of course adding
           a type annotation would make this work as well, and that one line instead of
           these five ;) -}
        addArrayProblemContext index = \zut ->
          ArrayE (Vec.map (NoProblem . getId) arr `Vec.unsafeUpd` [(index, zut)])
        evalArrayElement index (Id subExpr) =
          evalSubExpr (addArrayProblemContext index) subExpr
    Array <$> Vec.imapM evalArrayElement arr

  ApplyE (Id funcExpr) (Id argExpr) -> do
    func <- evalSubExpr (\z -> ApplyE z (NoProblem argExpr)) funcExpr
    case func of
      Function typ f -> do
        arg <- evalSubExpr (\z -> ApplyE (NoProblem funcExpr) z) argExpr
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

data Problematic f a
  = ProblemHere (Problem f)
  | ProblemWithin a
  | NoProblem Expr
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

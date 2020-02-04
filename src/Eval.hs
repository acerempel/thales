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
  { unEvalT :: ExceptT (Problem m) (ReaderT (Bindings m) m) a }
  deriving ( Monad, Functor, Applicative )

runEvalT :: EvalT m a -> Bindings m -> m (Either (Problem m) a)
runEvalT (EvalT m) = runReaderT (runExceptT m)

-- | Abort this evaluation with the given problem.
zutAlors :: Monad m => ProblemDescription m -> EvalT m a
zutAlors prob = EvalT (throwE (Problem Nowhere prob))

-- | If the second argument aborts with a problem, wrap the problem
-- with the first argument, which should represent
-- the expression that the caller of 'mapZut' is evaluating.
mapZut :: Monad m => Expr -> AddProblemContext -> EvalT m b -> EvalT m b
mapZut expr f (EvalT m) =
  EvalT $ withExceptT (problemAddContext f . problemSetSource expr) m

lookup :: Monad m => Name -> EvalT m (Maybe (Value m))
lookup n = EvalT (asks (Map.lookup n))

getBindings :: Monad m => EvalT m (Bindings m)
getBindings = EvalT ask

localBindings :: Monad m => (Bindings m -> Bindings m) -> EvalT m a -> EvalT m a
localBindings f (EvalT r) = EvalT (local f r)

liftEval :: Monad m => m a -> EvalT m a
liftEval m = EvalT (lift (lift m))

type AddProblemContext =
  ProblemWhere (ExprH ProblemWhere) -> ExprH ProblemWhere

evalSubExpr :: Monad m => AddProblemContext -> Expr -> EvalT m (Value m)
evalSubExpr f = evalExpr (Just f)

evalTopExpr :: Monad m => Expr -> EvalT m (Value m)
evalTopExpr = evalExpr Nothing

evalExpr :: Monad m => Maybe AddProblemContext -> Expr -> EvalT m (Value m)
evalExpr mContext expr =
 maybe id (mapZut expr) mContext $ case expr of

  NameE name -> do
    mVal <- lookup name
    case mVal of
      Just val -> return val
      Nothing -> zutAlors (NameNotFound name)

  FieldAccessE name (Id subExpr) -> do
    subVal <- evalSubExpr (FieldAccessE name) subExpr
    case subVal of
      Record rec ->
        let ohNo = zutAlors (FieldNotFound name subVal)
        in maybe ohNo return (Map.lookup name rec)
      _ ->
        zutAlors (NotARecord subVal)

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
            zutAlors (TypeMismatch (SomeValueType typ) arg)
      _ ->
        zutAlors (NotAFunction func)

data ProblemWhere a
  = ProblemHere Expr
  | ProblemWithin a
  | NoProblem Expr
  | Nowhere
  deriving Show

data ProblemDescription f
  = NameNotFound Name
  | FieldNotFound Name (Value f)
  | NotARecord (Value f)
  | NotAFunction (Value f)
  | TypeMismatch (SomeValueType f) (Value f)
  deriving Show

data Problem f = Problem
  { problemWhere :: ProblemWhere (ExprH ProblemWhere)
  , problemDescription :: ProblemDescription f }

problemAddContext :: AddProblemContext -> Problem f -> Problem f
problemAddContext add Problem{..} =
  Problem{problemWhere = ProblemWithin (add problemWhere), ..}

problemSetSource :: Expr -> Problem f -> Problem f
problemSetSource expr Problem{..} =
  Problem{problemWhere =
    case problemWhere of
      Nowhere -> ProblemHere expr
      _ -> problemWhere
  , .. }

{- TODO: how to write this? We will have to do the same thing with Statement
that we did with Expr, namely, make it a functor, so that it may contain either
a source Expr or an (Either ProblemWhere Value). -}
evalStatement :: Monad m => Statement -> EvalT m (Value m)
evalStatement _ = return undefined

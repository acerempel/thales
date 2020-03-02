{-# LANGUAGE NoMonoLocalBinds, StrictData #-}
module Eval
  ( EvalM, Bindings
  , Problem(..), ProblemWhere(..), ProblemDescription(..)
  , evalTopExpr, evalStatement, runEvalM
  )
where

import Control.Monad.Trans.Except
import Data.Functor.Classes
import Data.Traversable
import qualified Data.HashMap.Strict as Map
import Text.Show

import BaseMonad
import Function
import List (List)
import qualified List
import Syntax
import Value

-- | A 'Bindings' provides the bindings that are available at the top level
-- in a template. At the moment it is just a type synonym, but this will
-- probably change.
type Bindings = HashMap Name Value
type Functions = HashMap Name Function

newtype EvalM a = EvalM
  { unEvalM :: ExceptT Problem (ReaderT (Functions, Bindings) M) a }
  deriving ( Monad, Functor, Applicative )

runEvalM :: EvalM a -> Functions -> Bindings -> M (Either Problem a)
runEvalM (EvalM m) functions bindings =
  runReaderT (runExceptT m) (functions, bindings)

-- | Abort this evaluation with the given problem.
zutAlors :: ProblemDescription -> EvalM a
zutAlors prob = EvalM (throwE (Problem Nowhere prob))

lookup :: Name -> EvalM (Maybe Value)
lookup n = EvalM (asks (Map.lookup n . snd))

lookupFunction :: Name -> EvalM (Maybe Function)
lookupFunction n = EvalM (asks (Map.lookup n . fst))

localBindings :: (Bindings -> Bindings) -> EvalM a -> EvalM a
localBindings f (EvalM r) = EvalM (local (fmap f) r)

handleZut :: (Problem -> EvalM a) -> EvalM a -> EvalM a
handleZut handler (EvalM m) = EvalM (catchE m (unEvalM . handler))

liftEval :: M a -> EvalM a
liftEval m = EvalM (lift (lift m))

type AddProblemContext =
  ProblemWhere (ExprH ProblemWhere) -> ExprH ProblemWhere

evalSubExpr :: AddProblemContext -> Expr -> EvalM Value
evalSubExpr f = evalExpr (Just f)

evalTopExpr :: Expr -> EvalM Value
evalTopExpr = evalExpr Nothing

evalExpr :: Maybe AddProblemContext -> Expr -> EvalM Value
evalExpr mContext expr =
 maybe id mapZut mContext . addSource expr $ case expr of

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
          ArrayE
            ( List.unsafeUpdate index zut
            $ List.map (NoProblem . getId) arr)
        evalArrayElement index (Id subExpr) =
          evalSubExpr (addArrayProblemContext index) subExpr
    Array <$> List.imapA evalArrayElement arr

  ApplyE (Id funcExpr) (Id argExpr) ->
    case funcExpr of
      NameE funcName -> do
        func <- lookupFunction funcName
        case func of
          Just (Function typ f) -> do
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
          Just _ ->
            zutAlors (NotAFunction funcName)
          Nothing ->
            zutAlors (NameNotFound funcName)
      _ ->
        zutAlors (NotAFunction (error "Oh no!"))
 where
  mapZut f (EvalM m) =
    EvalM $ withExceptT (problemAddContext f) m
  addSource source (EvalM m) =
    EvalM $ withExceptT (problemSetSource source) m

data ProblemWhere a
  = ProblemHere Expr
  | ProblemWithin a
  | NoProblem Expr
  | Nowhere
  deriving ( Show, Eq )

instance Show1 ProblemWhere where
  liftShowsPrec showsPrecA _showListA prec = \case
    ProblemWithin nested ->
      -- TODO: use prec correctly!
      ("ProblemWithin (" <>)
      . showsPrecA prec nested
      . (')' :)
    ProblemHere expr ->
      ("ProblemHere (" <>)
      . showsPrec prec expr
      . (')' :)
    NoProblem expr ->
      ("NoProblem (" <>)
      . showsPrec prec expr
      . (')' :)
    Nowhere ->
      ("Nowhere" <>)

instance Eq1 ProblemWhere where
  liftEq eqA p1 p2 =
    case (p1, p2) of
      (ProblemHere e1, ProblemHere e2) ->
        e1 == e2
      (NoProblem e1, NoProblem e2) ->
        e1 == e2
      (Nowhere, Nowhere) ->
        True
      (ProblemWithin a1, ProblemWithin a2) ->
        eqA a1 a2
      _ ->
        False

data ProblemDescription
  = NameNotFound Name
  | FieldNotFound Name Value
  | NotARecord Value
  | NotAFunction Name
  | TypeMismatch SomeValueType Value
  deriving ( Show, Eq )

data Problem = Problem
  { problemWhere :: ProblemWhere (ExprH ProblemWhere)
  , problemDescription :: ProblemDescription }
  deriving ( Show, Eq )

problemAddContext :: AddProblemContext -> Problem -> Problem
problemAddContext add Problem{..} =
  Problem{problemWhere = ProblemWithin (add problemWhere), ..}

problemSetSource :: Expr -> Problem -> Problem
problemSetSource expr Problem{..} =
  Problem{problemWhere =
    case problemWhere of
      Nowhere -> ProblemHere expr
      _ -> problemWhere
  , .. }

literalToValue :: Literal -> Value
literalToValue = \case
  NumberL  n -> Number n
  StringL  s -> String s
  BooleanL b -> Boolean b

evalStatement :: Statement -> EvalM (List Value)
evalStatement = \case
  VerbatimS verb ->
    return $ List.singleton (Verbatim verb)
  ExprS _sp expr ->
    List.singleton <$> evalTopExpr expr
  ForS _sp var expr body -> do
    val <- evalTopExpr expr
    case val of
      Array vec ->
        List.concat <$>
        for vec (\item ->
          localBindings
          (Map.insert var item)
          (List.concat <$> for (List.fromList body) evalStatement))
      _ ->
        zutAlors (TypeMismatch (SomeValueType ArrayT) val)
  Optionally _sp var expr body -> do
    val <- handleZut (\_ -> return Nothing) (Just <$> evalTopExpr expr)
    case val of
      Nothing -> return List.empty
      Just v ->
        localBindings
        (Map.insert var v)
        (List.concat <$> for (List.fromList body) evalStatement)
  Optional sp expr ->
    let dummyName = "____" in
    evalStatement (Optionally sp dummyName expr [ExprS sp (NameE dummyName)])

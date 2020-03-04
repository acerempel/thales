module Eval
  ( EvalM, Bindings
  , Problem(..), ProblemWhere(..), ProblemDescription(..)
  , evalTopExpr, evalStatement, runEvalM
  )
where

import Data.Traversable
import qualified Data.HashMap.Strict as Map

import Eval.Expr
import List (List)
import qualified List
import Syntax
import Value

evalSubExpr :: AddProblemContext -> Expr -> EvalM Value
evalSubExpr f = evalExpr (Just f)

evalTopExpr :: Expr -> EvalM Value
evalTopExpr = evalExpr Nothing

evalExpr :: Maybe AddProblemContext -> Expr -> EvalM Value
evalExpr mContext expr =
 maybe id mapZut mContext . addProblemSource expr $ case expr of

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
    let addArrayProblemContext index = \zut ->
          ArrayE
            ( List.unsafeUpdate index zut
            $ List.map (NoProblem . getId) arr)
        evalArrayElement index (Id subExpr) =
          evalSubExpr (addArrayProblemContext index) subExpr
    Array <$> List.imapA evalArrayElement arr

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
        zutAlors (NotAnArray val)
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

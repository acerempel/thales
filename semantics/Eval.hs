module Eval
  ( ExprM, Bindings
  , Problem(..), ProblemWhere(..), ProblemDescription(..)
  , evalTopExpr, evalStatement, runExprM, runStmtM
  )
where

import qualified Data.HashMap.Strict as Map

import Bindings
import Eval.Expr
import Eval.Statement
import qualified List
import qualified NonEmptyText
import Output
import Syntax
import Value

evalSubExpr :: AddProblemContext -> Expr -> ExprM Value
evalSubExpr f = evalExpr (Just f)

evalTopExpr :: Expr -> ExprM Value
evalTopExpr = evalExpr Nothing

evalExpr :: Maybe AddProblemContext -> Expr -> ExprM Value
evalExpr mContext expr =
 maybe id mapZut mContext . addProblemSource expr $ case expr of

  NameE name -> do
    mVal <- lookupName name
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

evalStatement :: Statement -> StmtM ()
evalStatement = \case

  VerbatimS verb ->
    addOutput (Output.fromText (NonEmptyText.toText verb))

  ExprS _sp expr -> do
    text <- liftExprM $ do
      val <- evalTopExpr expr
      case val of
        String text ->
          return (Output.fromText text)
        _ ->
          zutAlors (NotText val)
    addOutput text

  ForS _sp var expr body -> do
    arr <- liftExprM $ do
      val <- evalTopExpr expr
      case val of
        Array vec ->
          return vec
        _ ->
          zutAlors (NotAnArray val)
    for_ arr $ \val ->
      for_ body $ \stmt ->
        addLocalBinding var val $ evalStatement stmt

  OptionallyS _sp expr mb_var body -> do
    mb_val <- liftExprM $
      handleZut (\_ -> return Nothing) (Just <$> evalTopExpr expr)
    whenJust mb_val $ \val ->
      for_ body $ \stmt ->
        maybe id (`addLocalBinding` val) mb_var $
        evalStatement stmt

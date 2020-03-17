module Eval
  ( ExprT, Bindings
  , Problem(..), ProblemWhere(..), ProblemDescription(..)
  , evalTopExpr, evalStatement, runExprT, runStmtT
  )
where

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as Map

import BaseMonad
import Bindings
import Eval.Expr
import Eval.Statement
import qualified List
import qualified NonEmptyText
import Output
import Syntax
import Value

evalSubExpr :: BaseMonad m => AddProblemContext -> Expr -> ExprT m Value
evalSubExpr f = evalExpr (Just f)

evalTopExpr :: BaseMonad m => Expr -> ExprT m Value
evalTopExpr = evalExpr Nothing

evalExpr :: BaseMonad m => Maybe AddProblemContext -> Expr -> ExprT m Value
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

  ListDirectoryE (Id subExpr) -> do
    path <- evalSubExpr (ListDirectoryE) subExpr
    case path of
      String str ->
        lift
        $ Array . List.map (String . Text.pack) . List.fromList
        <$> listDirectory (Text.unpack str)
      _ ->
        zutAlors (error "oh no!!!")

  FileE (Id subExpr) -> do
    path <- evalSubExpr (FileE) subExpr
    case path of
      String str ->
        zutAlors (error "urk!")
      _ ->
        zutAlors (error "ack!")


literalToValue :: Literal -> Value
literalToValue = \case
  NumberL  n -> Number n
  StringL  s -> String s
  BooleanL b -> Boolean b

evalStatement :: forall m. BaseMonad m => Statement -> StmtT m ()
evalStatement = \case

  VerbatimS verb ->
    addOutput (Output.fromText (NonEmptyText.toText verb))

  ExprS _sp expr -> do
    text <- liftExprT $ do
      val <- evalTopExpr expr :: ExprT m Value
      case val of
        String text ->
          return (Output.fromText text)
        _ ->
          zutAlors (NotText val)
    addOutput text

  ForS _sp var expr body -> do
    arr <- liftExprT $ do
      val <- evalTopExpr expr :: ExprT m Value
      case val of
        Array vec ->
          return vec
        _ ->
          zutAlors (NotAnArray val)
    for_ arr $ \val ->
      for_ body $ \stmt ->
        addLocalBinding var val $ evalStatement stmt

  OptionallyS _sp expr mb_var body -> do
    mb_val <- liftExprT $
      handleZut (\_ -> return Nothing) (Just <$> evalTopExpr expr :: ExprT m (Maybe Value))
    whenJust mb_val $ \val ->
      for_ body $ \stmt ->
        maybe id (`addLocalBinding` val) mb_var $
        evalStatement stmt

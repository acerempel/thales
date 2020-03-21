module Eval
  ( ExprT, Bindings
  , Problem(..), ProblemWhere(..), ProblemDescription(..)
  , evalTopExpr, evalStatement, runExprT, runStmtT
  )
where

import qualified Data.Text as Text
import Data.Traversable
import qualified Data.HashMap.Strict as Map
import System.FilePath

import {-# SOURCE #-} DependencyMonad
import Bindings
import Eval.Expr
import Eval.Statement
import qualified List
import qualified NonEmptyText
import Output
import Syntax
import Value

evalSubExpr :: DependencyMonad m => AddProblemContext -> FilePath -> Expr -> ExprT m Value
evalSubExpr f = evalExpr (Just f)

evalTopExpr :: DependencyMonad m => FilePath -> Expr -> ExprT m Value
evalTopExpr = evalExpr Nothing

evalExpr :: DependencyMonad m => Maybe AddProblemContext -> FilePath -> Expr -> ExprT m Value
evalExpr mContext dir expr =
 maybe id mapZut mContext . addProblemSource expr $ case expr of

  NameE name -> do
    mVal <- lookupName name
    case mVal of
      Just val -> return val
      Nothing -> zutAlors (NameNotFound name)

  FieldAccessE name (Id subExpr) -> do
    subVal <- evalSubExpr (FieldAccessE name) dir subExpr
    let ohNo = zutAlors (FieldNotFound name subVal)
    case subVal of
      Record rec ->
        maybe ohNo return (Map.lookup (fromName name) rec)
      ExternalRecord ft path -> do
        mb_val <- lift $ lookupField (Bindings <$> ft) path (fromName name)
        maybe ohNo return mb_val
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
          evalSubExpr (addArrayProblemContext index) dir subExpr
    Array <$> List.imapA evalArrayElement arr

  ListDirectoryE (Id subExpr) -> do
    path <- evalSubExpr ListDirectoryE dir subExpr
    case path of
      String str ->
        lift
        $ Array . List.map (String . Text.pack) . List.fromList
        <$> listDirectory (dir </> Text.unpack str)
      _ ->
        zutAlors (error "oh no!!!")

  FileE ft (Id subExpr) -> do
    -- TODO: make this comprehensible
    path <- evalSubExpr (FileE (NoProblem . getId <$> ft)) dir subExpr
    ft' <- traverse (evalSubExpr (\ft'' -> FileE (pure ft'') (NoProblem subExpr)) dir) (getId <$> ft)
    let ft'' = traverse (\val -> case val of { Record r -> Right r; _ -> Left "oh no!" }) ft'
    case (path, ft'') of
      (String str, Right rec) ->
        pure (ExternalRecord rec (Text.unpack str))
      _ ->
        zutAlors (error "ack!")

literalToValue :: Literal -> Value
literalToValue = \case
  NumberL  n -> Number n
  StringL  s -> String s
  BooleanL b -> Boolean b

evalStatement :: forall m. DependencyMonad m => Statement -> StmtT m ()
evalStatement = \case

  VerbatimS verb ->
    addOutput (Output.fromText (NonEmptyText.toText verb))

  ExprS sp expr -> do
    text <- liftExprT $ do
      val <- evalTopExpr (sourceDir sp) expr :: ExprT m Value
      case val of
        String text ->
          return (Output.fromText text)
        Output out ->
          return (Output.fromStorable out)
        _ ->
          zutAlors (NotText val)
    addOutput text

  ForS sp var expr body -> do
    arr <- liftExprT $ do
      val <- evalTopExpr (sourceDir sp) expr :: ExprT m Value
      case val of
        Array vec ->
          return vec
        _ ->
          zutAlors (NotAnArray val)
    for_ arr $ \val ->
      for_ body $ \stmt ->
        addLocalBinding var val $ evalStatement stmt

  OptionallyS sp expr mb_var body -> do
    mb_val <- liftExprT $
      handleZut (\_ -> return Nothing) (Just <$> evalTopExpr (sourceDir sp) expr :: ExprT m (Maybe Value))
    whenJust mb_val $ \val ->
      for_ body $ \stmt ->
        maybe id (`addLocalBinding` val) mb_var $
        evalStatement stmt

  LetS sp binds body -> do
    eval'd_binds <-
      for binds $ \(name, expr) -> liftExprT $
        (name,) <$> (evalTopExpr (sourceDir sp) expr :: ExprT m Value)
    addLocalBindings eval'd_binds $
      for_ body evalStatement

  where
    sourceDir = takeDirectory . sourceName

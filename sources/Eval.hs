{-# OPTIONS_GHC -Wno-name-shadowing #-}
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

evalSubExpr :: DependencyMonad m => AddProblemContext -> Expr -> ExprT m Value
evalSubExpr f = evalExpr (Just f)

evalTopExpr :: DependencyMonad m => Expr -> ExprT m Value
evalTopExpr = evalExpr Nothing

evalExpr :: DependencyMonad m => Maybe AddProblemContext -> Expr -> ExprT m Value
evalExpr mContext expr =
 maybe id mapZut mContext . addProblemSource expr $ case expr of

  NameE name -> do
    mVal <- lookupName name
    case mVal of
      Just val -> return val
      Nothing -> zutAlors (NameNotFound name)

  FieldAccessE name (Id subExpr) -> do
    subVal <- evalSubExpr (FieldAccessE name) subExpr
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
            $ List.map (NoProblem . getId) arr )
        evalArrayElement index (Id subExpr) =
          evalSubExpr (addArrayProblemContext index) subExpr
    Array <$> List.imapA evalArrayElement arr

  RecordE bindings -> do
    -- TODO: preserve context!
    Record . Map.fromList <$> traverse evalBinding bindings

  ListDirectoryE (Id subExpr) -> do
    path <- evalSubExpr ListDirectoryE subExpr
    dir <- getTemplateDirectory
    case path of
      String str ->
        lift
        $ Array . List.map (String . Text.pack) . List.fromList
        <$> listDirectory (dir </> Text.unpack str)
      _ ->
        zutAlors (error "oh no!!!")

  FileE ft (Id subExpr) -> do
    -- TODO: make this comprehensible
    path <- evalSubExpr (FileE (NoProblem . getId <$> ft)) subExpr
    dir <- getTemplateDirectory
    let addFileTypeProblemContext a =
          FileE (a <$ ft) (NoProblem subExpr)
    ft' <- traverse (evalSubExpr addFileTypeProblemContext) (getId <$> ft)
    let ft'' = traverse (\val -> case val of { Record r -> Just r; _ -> Nothing }) ft'
    case (path, ft'') of
      (String str, Just rec) ->
        pure (ExternalRecord rec (dir </> Text.unpack str))
      _ ->
        zutAlors (error "ack!")

literalToValue :: Literal -> Value
literalToValue = \case
  NumberL  n -> Number n
  StringL  s -> String s
  BooleanL b -> Boolean b

evalBinding :: DependencyMonad m => RecordBinding Id -> ExprT m (Text, Value)
evalBinding bind =
  let (name, expr) = expandBinding bind
  in (fromName name,) <$> evalSubExpr (\e -> RecordE [FieldAssignment name e]) expr -- TODO
 where
  expandBinding (FieldPun name) =
    (name, NameE name)
  expandBinding (FieldAssignment name (Id expr)) =
    (name, expr)

evalStatement :: forall m. DependencyMonad m => Statement -> StmtT m ()
evalStatement = \case

  VerbatimS verb ->
    addOutput (Output.fromText (NonEmptyText.toText verb))

  ExprS sp expr -> do
    text <- liftExprT (sourceDir sp) $ do
      val <- evalTopExpr expr :: ExprT m Value
      case val of
        String text ->
          return (Output.fromText text)
        Output out ->
          return (Output.fromStorable out)
        _ ->
          zutAlors (NotText val)
    addOutput text

  ForS sp var expr body -> do
    arr <- liftExprT (sourceDir sp) $ do
      val <- evalTopExpr expr :: ExprT m Value
      case val of
        Array vec ->
          return vec
        _ ->
          zutAlors (NotAnArray val)
    for_ arr $ \val ->
      for_ body $ \stmt ->
        addLocalBinding var val $ evalStatement stmt

  OptionallyS sp expr mb_var body -> do
    mb_val <- liftExprT (sourceDir sp) $
      handleZut (\_ -> return Nothing) (Just <$> evalTopExpr expr :: ExprT m (Maybe Value))
    whenJust mb_val $ \val ->
      for_ body $ \stmt ->
        maybe id (`addLocalBinding` val) mb_var $
        evalStatement stmt

  LetS sp binds body -> do
    eval'd_binds <-
      for binds $ \(name, expr) -> liftExprT (sourceDir sp) $
        (name,) <$> (evalTopExpr expr :: ExprT m Value)
    addLocalBindings eval'd_binds $
      for_ body evalStatement

  ExportS sp binds -> do
    eval'd_binds <-
      for binds $ \bind -> liftExprT (sourceDir sp) $
        first Name <$> evalBinding bind :: StmtT m (Name, Value)
    addBindings eval'd_binds

  where
    sourceDir = takeDirectory . sourceName

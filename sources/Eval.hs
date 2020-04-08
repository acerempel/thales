{-# OPTIONS_GHC -Wno-name-shadowing -Wno-missing-signatures -Wmissing-exported-signatures #-}
{-# LANGUAGE OverloadedLists #-}
module Eval
  ( ExprT, Bindings
  , evalTopExpr, evalStatement, runExprT, runStmtT
  , evalTemplate
  )
where

import Data.DList (DList)
import qualified Data.Text as Text
import Data.Traversable
import qualified Data.HashMap.Strict as Map
import Development.Shake.FilePath

import {-# SOURCE #-} DependencyMonad
import Bindings
import Eval.Expr
import Eval.Function
import Eval.Statement
import KnownFunction
import List (List)
import qualified List
import qualified NonEmptyText
import Output
import Parse
import Problem
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
      Nothing -> do
        namesInScope <- getLocalNames
        zutAlors (ProblemNameNotFound name namesInScope)

  FieldAccessE name (Id subExpr) -> do
    subVal <- evalSubExpr (FieldAccessE name) subExpr
    let ohNo available = zutAlors (ProblemFieldNotFound name available)
    case subVal of
      Record rec ->
        case Map.lookup (fromName name) rec of
          Nothing ->
            ohNo (coerce (Map.keys rec))
          Just val ->
            return val
      ExternalRecord ft path -> do
        mb_val <- lift $ lookupField ft path (fromName name)
        case mb_val of
          Nothing -> do
            keys <- lift $ listFields ft path
            ohNo keys
          Just val ->
            return val
      _ ->
        typeMismatch subVal [RecordT]

  LiteralE lit ->
    return (literalToValue lit)

  ArrayE arr -> do
    Array <$> evalList ArrayE arr

  RecordE bindings -> do
    -- TODO: preserve context!
    Record . Map.fromList <$> traverse evalBinding bindings

  FunctionCallE name args -> do
    case find ((== name) . fst) knownFunctions of
      Nothing ->
        zutAlors (ProblemUnknownFunction name (map fst knownFunctions))
      Just (_, func) -> do
        arg_vals <- evalList (FunctionCallE name) args
        result <- evalFunctionM func (toList arg_vals)
        case result of
          Pure val ->
            pure val
          Action act ->
            case act of
              ListDirectory dir -> do
                tplDir <- getTemplateDirectory
                lift $ Array . List.map (String . Text.pack) . List.fromList
                  <$> listDirectory (tplDir </> dir)
              LoadYaml fp -> do
                tplDir <- getTemplateDirectory
                pure $ ExternalRecord YamlFile (tplDir </> fp)
              LoadMarkdown fp -> do
                tplDir <- getTemplateDirectory
                pure $ ExternalRecord MarkdownFile (tplDir </> fp)
              LoadTemplate fp binds -> do
                tplDir <- getTemplateDirectory
                delims <- getTemplateDelimiters
                pure $ ExternalRecord (TemplateFile delims binds) (tplDir </> fp)

literalToValue :: Literal -> Value
literalToValue = \case
  NumberL  n -> Number n
  StringL  s -> String s
  BooleanL b -> Boolean b

knownFunctions :: [(Name, FunctionM [Value] ProblemDescription FunctionResult)]
knownFunctions =
  [ ("append", Pure <$> appendFunction)
  , ("list-directory", Action <$> listDirectoryFunction)
  , (Name loadYamlFunctionName, Action <$> loadYamlFunction)
  , (Name loadMarkdownFunctionName, Action <$> loadMarkdownFunction)
  ]

appendFunction =
  withTwoArguments $
    String <$> liftF2 (<>) textArgument textArgument <|>
    Array <$> liftF2 (<>) arrayArgument arrayArgument

listDirectoryFunction =
  withOneArgument $
    ListDirectory . Text.unpack <$> liftF1 textArgument

loadYamlFunction =
  withOneArgument $
    LoadYaml . Text.unpack <$> liftF1 textArgument

loadMarkdownFunction =
  withOneArgument $
    LoadMarkdown . Text.unpack <$> liftF1 textArgument

data FunctionResult
  = Pure Value
  | Action FunctionAction

data FunctionAction
  = ListDirectory FilePath
  | LoadYaml FilePath
  | LoadMarkdown FilePath
  | LoadTemplate FilePath (HashMap Text Value)

evalList ::
  DependencyMonad m =>
  (List (ProblemWhere (ExprH ProblemWhere)) -> ExprH ProblemWhere) ->
  List (Id Expr) ->
  ExprT m (List Value)
evalList constr arr = List.imapA evalArrayElement arr
 where
  evalArrayElement index (Id subExpr) =
    evalSubExpr (addArrayProblemContext index) subExpr
  addArrayProblemContext index = \zut ->
      constr
        ( List.unsafeUpdate index zut
        $ List.map (NoProblem . getId) arr )

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

  ExprS _sp expr -> do
    text <- liftExprT $ do
      val <- evalTopExpr expr :: ExprT m Value
      case val of
        String text ->
          return (Output.fromText text)
        Output out ->
          return (Output.fromStorable out)
        _ ->
          typeMismatch val [TextT, OutputT]
    addOutput text

  ForS _sp var expr body -> do
    arr <- liftExprT $ do
      val <- evalTopExpr expr :: ExprT m Value
      case val of
        Array vec ->
          return vec
        _ ->
          typeMismatch val [ArrayT]
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

  LetS _sp binds body -> do
    eval'd_binds <-
      for binds $ \(name, expr) -> liftExprT $
        (name,) <$> (evalTopExpr expr :: ExprT m Value)
    addLocalBindings eval'd_binds $
      for_ body evalStatement

  ExportS _sp binds -> do
    eval'd_binds <-
      for binds $ \bind -> liftExprT $
        first Name <$> evalBinding bind :: StmtT m (Name, Value)
    addBindings eval'd_binds

evalTemplate :: DependencyMonad m => ParsedTemplate -> Bindings -> m (Either (DList Problem) (Bindings, Output))
evalTemplate ParsedTemplate{..} =
  runStmtT (traverse_ evalStatement templateStatements) templatePath templateDelimiters

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

import DependencyMonad.Class
import Bindings
import Eval.Expr
import Eval.Statement
import Eval.Function
import KnownFunction
import List (List)
import qualified List
import Output
import Parse
import Eval.Problem
import Syntax
import Value

evalSubExpr :: DependencyMonad m => AddProblemContext -> Expr -> ExprT m Value
evalSubExpr f = evalExpr (Just f)

evalTopExpr :: DependencyMonad m => Expr -> ExprT m Value
evalTopExpr = evalExpr Nothing

{-# SCC evalExpr #-}
evalExpr :: DependencyMonad m => Maybe AddProblemContext -> Expr -> ExprT m Value
evalExpr mContext expr =
 maybe id mapZut mContext $ case expr of

  NameE name -> do
    mVal <- lookupName name
    case mVal of
      Just val -> return val
      Nothing -> do
        namesInScope <- getLocalNames
        exprProblem (NameNotFound name namesInScope)

  FieldAccessE name opt@(IsOptional isOpt) (Rec subExpr) -> do
    subVal <- evalSubExpr (FieldAccessE name opt) subExpr
    let ohNo available = exprProblem (FieldAccessProblem name subExpr opt (FieldAccessFieldNotFound available))
    case subVal of
      Record rec ->
        case Map.lookup (fromName name) rec of
          Nothing
            | isOpt ->
              return Empty
            | otherwise ->
              ohNo (coerce (Map.keys rec))
          Just val ->
            return val
      LoadedDoc (DocInfo ft path) -> do
        mb_val <- lift $ lookupField ft path (fromName name)
        case mb_val of
          Nothing
            | isOpt ->
              return Empty
            | otherwise -> do
              keys <- lift $ listFields ft path
              ohNo keys
          Just val ->
            return val
      Empty ->
        return Empty
      _ ->
        exprProblem (FieldAccessProblem name subExpr opt (FieldAccessNotARecord subVal))

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
        exprProblem
          (FunctionCallProblem name (coerce args)
            (FunctionDoesNotExist (map fst knownFunctions)))
      Just (_, func) -> do
        arg_vals <- evalList
          (FunctionCallE name . toList)
          (List.fromList args)
        result <-
          case applySignature func (toList arg_vals) of
            Left err -> exprProblem (FunctionCallProblem name (coerce args) err)
            Right a -> return a
        case result of
          Pure val ->
            pure val
          Action act ->
            case act of
              ListDirectory dir ->
                lift $ Array . List.map (String . Text.pack) . List.fromList
                  <$> listDirectory dir
              LoadYaml fp ->
                pure $ LoadedDoc $ DocInfo YamlFile fp
              LoadTemplate fp binds -> do
                pure $ LoadedDoc $ DocInfo (TemplateFile binds) fp

literalToValue :: Literal -> Value
literalToValue = \case
  NumberL  n -> Number n
  StringL  s -> String s
  BooleanL b -> Boolean b
  EmptyL     -> Empty

knownFunctions :: [(Name, Signature FunctionResult)]
knownFunctions =
  [ ("concat", Pure <$> concatFunction)
  , ("list-directory", Action <$> listDirectoryFunction)
  , (Name loadYamlFunctionName, Action <$> loadYamlFunction)
  , (Name loadTemplateFunctionName, Action <$> loadTemplateFunction)
  , ("url", Pure <$> urlFunction)
  ]

concatFunction =
    String . mconcat <$> restOfArguments (orEmpty TextT) <|>
    Array . mconcat <$> restOfArguments (orEmpty ArrayT) <|>
    Record . mconcat <$> restOfArguments (orEmpty RecordT)

orEmpty :: Monoid t => ValueType t -> Signature t
orEmpty =
  fmap (either (const mempty) id) . eitherArgument EmptyT

listDirectoryFunction =
    ListDirectory . Text.unpack <$> argument TextT

loadYamlFunction =
    LoadYaml . Text.unpack <$> argument TextT

loadTemplateFunction =
    liftA2 LoadTemplate (Text.unpack <$> argument TextT) (orEmpty RecordT <|> pure Map.empty)

urlFunction =
  String . getUrlText <$> argument DocumentT
 where
  getUrlText DocInfo{ docFilePath }
    | docFPText == "index.html"
      = ""
    | Just prefix <- "/index.html" `Text.stripSuffix` docFPText
      = prefix
    | Just prefix <- ".html" `Text.stripSuffix` docFPText
      = prefix
    | otherwise
      = docFPText
    where docFPText = Text.pack docFilePath

data FunctionResult
  = Pure Value
  | Action FunctionAction

data FunctionAction
  = ListDirectory FilePath
  | LoadYaml FilePath
  | LoadTemplate FilePath (HashMap Text Value)

evalList ::
  DependencyMonad m =>
  (List ExprProblemInContext -> ExprF ExprProblemInContext) ->
  List (Rec ExprF) ->
  ExprT m (List Value)
evalList constr arr = List.imapA evalArrayElement arr
 where
  evalArrayElement index (Rec subExpr) =
    evalSubExpr (addArrayProblemContext index) subExpr
  addArrayProblemContext index = \zut ->
      constr
        ( List.unsafeUpdate index zut
        $ List.map (OK . unRec) arr )

evalBinding :: DependencyMonad m => RecordBinding (Rec ExprF) -> ExprT m (Text, Value)
evalBinding bind =
  let (name, expr) = expandBinding bind
  in (fromName name,) <$> evalSubExpr (\e -> RecordE [FieldAssignment name e]) expr -- TODO
 where
  expandBinding (FieldPun name) =
    (name, NameE name)
  expandBinding (FieldAssignment name (Rec expr)) =
    (name, expr)

{-# SCC evalStatement #-}
evalStatement :: forall m. DependencyMonad m => Statement -> StmtT m ()
evalStatement = \case

  VerbatimS char verb -> do
    addOutput (Output.preEscapedSingleton char)
    addOutput (Output.preEscapedFromText verb)

  ExprS sp expr -> do
    text <- do
      val <- liftExprT (ExprProblem sp) (evalTopExpr expr :: ExprT m Value)
      case val of
        String text ->
          return (Output.fromText text)
        Empty ->
          return mempty
        LoadedDoc (DocInfo filetype filepath) -> do
          liftExprT
            (ExprProblem sp)
            (lift (getBody filetype filepath))
        _ ->
          stmtProblem (ExprIsNotOutputable sp expr val)
    addOutput text

  ForS sp var expr body -> do
    arr <- do
      val <- liftExprT (ForProblem sp . ForExprProblem var) (evalTopExpr expr :: ExprT m Value)
      case val of
        Array vec ->
          return vec
        Empty ->
          return mempty
        _ ->
          stmtProblem (ForProblem sp (ForNotAnArray var expr val))
    for_ arr $ \val ->
      for_ body $ \stmt ->
        addLocalBinding var val $ evalStatement stmt

  OptionallyS sp expr mb_var body -> do
    mb_val <- liftExprT (OptionallyExprProblem sp mb_var) $ do
      val <- evalTopExpr expr :: ExprT m Value
      case val of
        Empty -> return Nothing
        val' -> return (Just val')
    whenJust mb_val $ \val ->
      for_ body $ \stmt ->
        maybe id (`addLocalBinding` val) mb_var $
        evalStatement stmt

  LetS sp binds -> do
    eval'd_binds <-
      for binds $ \(name, expr) -> liftExprT
        (\prob -> LetProblem sp [(name, prob)]) $ -- TODO correct context
        (name,) <$> (evalTopExpr expr :: ExprT m Value)
    addTopBindings eval'd_binds

  LetInS sp binds body -> do
    eval'd_binds <-
      for binds $ \(name, expr) -> liftExprT
        (\prob -> LetProblem sp [(name, prob)]) $ -- TODO correct context
        (name,) <$> (evalTopExpr expr :: ExprT m Value)
    addLocalBindings eval'd_binds $
      for_ body evalStatement

  ExportS sp binds -> do
    eval'd_binds <-
      for binds $ \bind -> liftExprT
        (\_ -> ExportProblem sp [Left (coerce bind)]) $ -- TODO what is going on here?
        first Name <$> evalBinding bind :: StmtT m (Name, Value)
    addBindings eval'd_binds

  IncludeBodyS sp expr ->
    evalStatement (ExprS sp expr)

evalTemplate :: DependencyMonad m => ParsedTemplate -> Bindings -> m (Either (DList StmtProblem) (Bindings, Output))
evalTemplate ParsedTemplate{..} =
  runStmtT (traverse_ evalStatement templateStatements)

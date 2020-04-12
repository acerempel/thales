{-# LANGUAGE OverloadedLists #-}
module Eval.Problem
  ( ExprProblemInContext(..)
  , ExprProblem(..), StmtProblem(..)
  , FunctionCallProblem(..), FieldAccessProblem(..)
  , RecordBindingProblem(..)
  , ForProblem(..), IncludeBodyProblem(..)
  , TypeMismatch(..), ArgumentTypeMismatches(..)
  , WrongNumberOfArguments(..), InsufficientArguments(..)
  , AddProblemContext
  , Markup(..), markupToAnsi
  , displayStmtProblem
  )
where

import Prelude hiding (group)

import Data.DList (DList)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal (unsafeTextWithoutNewlines)
import Data.Text.Prettyprint.Doc.Render.Terminal
import Text.Megaparsec (unPos)

import Syntax
import Syntax.Display
import Value

data Markup = Problematic | Heading

markupToAnsi :: Markup -> AnsiStyle
markupToAnsi = \case
  Problematic -> color Magenta <> bold
  Heading -> bold

data TypeMismatch = TypeMismatch Value (DList SomeValueType)
  deriving ( Show, Eq )

-- | Note that this instance assumes the 'Value' is the same!
instance Semigroup TypeMismatch where
  (TypeMismatch val types1) <> (TypeMismatch _ types2) =
    TypeMismatch val (types1 <> types2)

newtype ArgumentTypeMismatches = ArgumentTypeMismatches
  { fromArgumentTypeMismatches :: IntMap TypeMismatch }
  deriving ( Show, Eq )

instance Semigroup ArgumentTypeMismatches where
  a1 <> a2 = ArgumentTypeMismatches $
    IntMap.unionWith (<>) (fromArgumentTypeMismatches a1) (fromArgumentTypeMismatches a2)

instance Monoid ArgumentTypeMismatches where
  mempty = ArgumentTypeMismatches IntMap.empty

type AddProblemContext =
  ExprProblemInContext -> ExprF ExprProblemInContext

data ExprProblemInContext
  = Here ExprProblem
  | OK Expr
  | Within (ExprF ExprProblemInContext)
  deriving Show

displayExprProblemInContext :: ExprProblemInContext -> Doc Markup
displayExprProblemInContext = \case
  Here prob -> displayExprProblem prob
  OK expr -> displayExpr expr
  Within expf -> displayExprF displayExprProblemInContext expf

data StmtProblem
  = ExprProblem SourcePos ExprProblemInContext
  | ExprIsNotOutputable SourcePos Expr Value
  | ForProblem SourcePos ForProblem
  | IncludeBodyProblem SourcePos IncludeBodyProblem
  | OptionallyExprProblem SourcePos (Maybe Name) ExprProblemInContext
  | ExportProblem SourcePos [Either (RecordBinding Expr) RecordBindingProblem]
  | LetProblem SourcePos [(Name, ExprProblemInContext)]
  deriving Show

data ForProblem
  = ForNotAnArray Name Expr Value
  | ForExprProblem Name ExprProblemInContext
  deriving Show

data IncludeBodyProblem
  = IncludeBodyNotADocument Expr Value
  | IncludeBodyExprProblem ExprProblemInContext
  deriving Show

displayStmtProblem :: StmtProblem -> Doc Markup
displayStmtProblem = \case
  ExprProblem sp epic ->
    displayError sp $ displayExprProblemInContext epic
  _ -> error "nyi!"

data ExprProblem
  = FunctionCallProblem Name [Expr] FunctionCallProblem
  | FieldAccessProblem Name Expr FieldAccessProblem
  | RecordLiteralProblem [Either (RecordBinding Expr) RecordBindingProblem]
  | NameNotFound Name [Name]
  deriving Show

data FunctionCallProblem
  = FunctionDoesNotExist [Name]
  | FunctionArgumentTypeMismatches ArgumentTypeMismatches
  | FunctionInsufficientArguments InsufficientArguments
  | FunctionWrongNumberOfArguments WrongNumberOfArguments
  deriving Show

data FieldAccessProblem
  = FieldAccessFieldNotFound [Name]
  | FieldAccessNotARecord Value
  deriving Show

data RecordBindingProblem
  = RecordBindingNameNotFound Name
  deriving Show

data WrongNumberOfArguments
  = WrongNumberOfArguments { expected :: Int, actual :: Int }
  deriving Show

newtype InsufficientArguments
  = InsufficientArguments Int
  deriving Show

displayExprProblem :: ExprProblem -> Doc Markup
displayExprProblem = \case
  NameNotFound name available ->
    let nnfMessage =
          "Name not found!" :| availMessage
        availMessage =
          if null available
             then []
             else one $ nest 2 ("These names are available:" <> line <>
              fillSep (punctuate comma (map displayName available)))
     in withErrorMessage (displayName name) nnfMessage
  FieldAccessProblem name expr prob ->
    case prob of
      FieldAccessFieldNotFound avail ->
        let fnfMessage =
              [ "This field does not exist!"
              , nest 2 ("The record" <> softline <> displayExpr expr)
              , nest 2 ("contains these fields:" <> line <>
                fillSep (punctuate comma (map displayName avail))) ]
         in displayFieldAccess (displayExpr expr)
              (withErrorMessage (displayName name) fnfMessage)
      FieldAccessNotARecord val ->
        let narMessage =
              [ "This value does not have fields,"
              , "being neither a record nor a document,"
              , "but rather a " <> displayType (valueType val) <> comma
              , nest 2 ("namely" <> softline <> displayValue val) ]
         in displayFieldAccessLineBreak Must
          (withErrorMessage (displayExpr expr) narMessage)
          (displayName name)
  FunctionCallProblem name args prob ->
    case prob of
      FunctionDoesNotExist knownFunctions ->
        let message =
              [ "That's not the name of a function!"
              , nest 2 $ "These functions exist:" <> line <>
                fillSep (punctuate comma (map displayName knownFunctions)) ]
         in displayFunctionCall
          (withErrorMessage (displayName name) message)
          (map displayExpr args)
      _ ->
        withErrorMessage
          (displayFunctionCall (displayName name) (map displayExpr args))
          [ "An error has occurred!" ]
  _ -> error "not yet implemented!"

withErrorMessage :: Doc Markup -> NonEmpty (Doc Markup) -> Doc Markup
withErrorMessage problematic (m1 :| messageLines) =
    align $
      width
        (annotate Problematic problematic)
        (\w ->
          annotate Heading (unsafeTextWithoutNewlines " ← " <> m1) <>
          hardline <> annotate Problematic (unsafeTextWithoutNewlines (Text.replicate w "^")) <>
            "   " <> annotate Heading (align (fillSep messageLines))
        )

displayError :: SourcePos -> Doc any -> Doc any
displayError SourcePos{..} doc =
  pretty sourceName <> ":" <>
  pretty (unPos sourceLine) <> ":" <>
  pretty (unPos sourceColumn) <>
  line <> indent 2 doc

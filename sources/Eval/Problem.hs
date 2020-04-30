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

import qualified Data.DList as DList
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

data TypeMismatch = TypeMismatch Value (HashSet SomeValueType)
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
  ExprIsNotOutputable sp expr val ->
    displayError sp $
      nest 2 ("The expression" <> line <> displayExpr expr) <> line <>
      nest 2 ("whose result is" <> line <> displayValue val) <> line <>
      "cannot be spliced into a template;" <> softline <>
      "it must be a text or empty"
  ForProblem sp _ -> displayError sp "for problem"
  IncludeBodyProblem sp _ -> displayError sp "include body problem"
  OptionallyExprProblem sp _ _ -> displayError sp "optionally problem"
  ExportProblem sp _ -> displayError sp "provide problem"
  LetProblem sp _ -> displayError sp "let problem"

data ExprProblem
  = FunctionCallProblem Name [Expr] FunctionCallProblem
  | FieldAccessProblem Name Expr IsOptional FieldAccessProblem
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
  FieldAccessProblem name expr opt prob ->
    case prob of
      FieldAccessFieldNotFound avail ->
        let fnfMessage =
              [ "This field does not exist!"
              , nest 2 ("The record" <> softline <> displayExpr expr)
              , nest 2 ("contains these fields:" <> line <>
                fillSep (punctuate comma (map displayName avail))) ]
         in displayFieldAccess (displayExpr expr)
              (withErrorMessage (displayName name) fnfMessage)
              (displayOptionality opt)
      FieldAccessNotARecord val ->
        let narMessage =
              [ "This value does not have fields,"
              , "being neither a record nor a document,"
              , "but rather a " <> displayType (valueType val) <> comma
              , nest 2 ("namely" <> softline <> displayValue val) ]
         in displayFieldAccessLineBreak Must
          (withErrorMessage (displayExpr expr) narMessage)
          (displayName name)
          (displayOptionality opt)
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

      FunctionArgumentTypeMismatches (ArgumentTypeMismatches errors) ->
        displayFunctionCallLineBreak (Should Tight) (displayName name) $
          DList.toList . fst $
          foldl' go (mempty,1) args

        where
          go (acc,n) a =
            let next =
                  fromMaybe (displayExpr a) (displayTypeMismatch n a <$> IntMap.lookup n errors)
             in (acc <> DList.singleton next,n+1)

          displayTypeMismatch n a (TypeMismatch val types) =
            withErrorMessage (displayExpr a) (typeMismatchErrorMessage n val types)

          typeMismatchErrorMessage :: Int -> Value -> HashSet SomeValueType -> NonEmpty (Doc Markup)
          typeMismatchErrorMessage n val types =
            [ "The" <+> ordinal n <+> "argument"
            , "is a" <+> displayType (valueType val) <> comma
            , nest 2 ("namely" <> softline <> displayValue val <> comma)
            , nest 2 (expectedTypes types) ]

          expectedTypes types =
            case toList types of
              [] -> error "no types!" -- TODO use NonEmpty
              ty1:tys ->
                "but was expected to be a" <+> displayType ty1
                  <> foldMap ((("," <> softline <> "or a ") <>) . displayType) tys

          ordinal n = case n of
            1 -> "1st"; 2 -> "2nd"; 3 -> "3rd"
            4 -> "4th"; 5 -> "5th"; 6 -> "6th"
            7 -> "7th"; 8 -> "8th"; 9 -> "9th"
            10 -> "10th"
            _ -> error ("weird number of arguments: " <> show n)

      FunctionInsufficientArguments (InsufficientArguments n) ->
        withErrorMessage
          (displayFunctionCall (displayName name) (map displayExpr args))
          [ "Insufficient arguments!","Only got " <> pretty n ]

      FunctionWrongNumberOfArguments WrongNumberOfArguments{expected,actual} ->
        withErrorMessage
          (displayFunctionCall (displayName name) (map displayExpr args))
          [ "Wrong num of args!","Wanted " <> pretty expected <> comma, "got " <> pretty actual ]

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

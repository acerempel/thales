{-|
Description : Pretty-printing of abstract syntax trees.

This module defines the 'Display' class for pretty-printing, and instances for
template abstract syntax trees and their component types.
-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wmissing-exported-signatures #-}
module Display
  ( Display(..), Display1(..), DisplayH(..)
  , Markup(..)
  )
where

import Prelude hiding (group)

import Data.Text.Prettyprint.Doc
import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text

import KnownFunction
import qualified List
import List (List)
import Problem
import Syntax
import Value

data Markup = Problematic | Heading

class Display a where
  -- | Display something.
  display :: a -> Doc Markup
  default display :: (a ~ h f, DisplayH h, Display1 f) => a -> Doc Markup
  display = hoistDisplay

instance Display1 f => Display (ExprH f)
instance Display1 f => Display (RecordBinding f)

instance Display a => Display1 (Const a) where
  liftedDisplay = display . getConst

-- | For type constructors that can be displayed whenever their type parameter
-- can be displayed. Cf. 'Data.Functor.Classes.Show1' and similar.
class Display1 (f :: Type -> Type) where
  liftedDisplay :: Display a => f a -> Doc Markup

-- | For type constructors which can be displayed wherever their type parameter
-- is 'Display1'. The ‘H’ is for ‘higher-order’. It's needed for the sake of
-- displaying an 'ExprH', which has a higher-kinded type parameter.
--
-- I wrote this before I was using @-XQuantifiedConstraints@ – with that
-- language extension, this class would not be needed.
class DisplayH (h :: (Type -> Type) -> Type) where
  hoistDisplay :: Display1 f => h f -> Doc Markup

instance DisplayH ExprH where
  hoistDisplay = \case
    LiteralE lit ->
      display lit
    ArrayE vec ->
      displayList liftedDisplay vec
    FieldAccessE n a ->
      group $ nest 2 $ liftedDisplay a <> softline' <> dot <> display n
    NameE n ->
      display n
    RecordE binds ->
      displayRecord binds
    FunctionCallE name args ->
      nest 2 $ cat
          [ display name
          , parens $
            align . sep . punctuate comma $
            toList . List.map liftedDisplay $ args
          ]

instance DisplayH RecordBinding where
  hoistDisplay = \case
    FieldPun n ->
      display n
    FieldAssignment n expr ->
      nest 2 $ display n <+> equals <+> softline <+> liftedDisplay expr

instance Display1 Id where
  liftedDisplay (Id a) =
    display a

instance Display Literal where
  display = \case
    NumberL n -> unsafeViaShow n
    StringL s -> viaShow s
    BooleanL b -> pretty b

instance Display Name where
  display (Name net) =
    pretty net

instance Display Problem where
  display Problem{ problemWhere, problemDescription } =
    vsep
      [ "Zut alors!"
      , indent 2 $
        vsep
          [ display problemDescription
          , nest 2 $ "In this expression:" <> line <> liftedDisplay problemWhere ]
      ]

instance Display1 ProblemWhere where
  liftedDisplay = \case
    ProblemHere expr ->
      annotate Problematic $ display expr
    NoProblem expr ->
      display expr
    ProblemWithin inner ->
      display inner
    Nowhere ->
      mempty

instance Display ProblemDescription where
  display = \case
    ProblemTypeMismatch tm -> display tm
    ProblemWrongNumberOfArguments wn -> display wn
    ProblemArgumentErrors aes -> display aes
    ProblemNameNotFound name namesInScope ->
      errorMessage ("Name not found: " <> display name) $
        "‘" <> display name <> "’" <+> "is not available here;" <> softline
        <> nest 2 ("these names are available:" <> softline
        <> fillSep (punctuate comma (map display namesInScope)))

instance Display TypeMismatch where
  display (TypeMismatch val types) =
    errorMessage "Type mismatch!" $
      nest 2 ("The value" <+> display val)
      <> softline <> "is a" <+> display (valueType val) <> comma
      <> softline <> nest 2
        ("but was expected to have one of these types:" <> softline
         <> fillSep (punctuate comma (map display (toList types))))

instance Display WrongNumberOfArguments where
  display WrongNumberOfArguments{ expected, actual } =
    errorMessage "Wrong number of arguments!" $
      "Expected" <+> pretty expected <> comma <> softline
      <> "but" <+> pretty actual <+> "were given."

instance Display ArgumentErrors where
  display (ArgumentErrors errors) =
    vsep . map dispArgErr . IntMap.toList $ errors
    where
      dispArgErr = \(n, (TypeMismatch val types)) ->
        nest 2 $ "The" <+> ordinal n <+> "argument," <> softline
          <> nest 2 ("namely" <> softline <> display val <> comma) <> softline
          <> "is a" <+> display (valueType val) <> comma <> softline
          <> nest 2
            ("but was expected to have one of these types:" <> softline
             <> sep (punctuate comma (map display (toList types))))
      ordinal n = case n of
        1 -> "1st"; 2 -> "2nd"; 3 -> "3rd"
        4 -> "4th"; 5 -> "5th"; 6 -> "6th"
        7 -> "7th"; 8 -> "8th"; 9 -> "9th"
        10 -> "10th"
        _ -> error ("weird number of arguments: " <> show n)

errorMessage heading body =
  nest 2 $ annotate Heading heading <> line <> body

instance Display Value where
  display = \case
    Number n -> unsafeViaShow n
    String s -> viaShow s
    Boolean b -> pretty b
    Array a -> displayList display a
    Record r ->
      let pairToBinding (n,v) = FieldAssignment (Name n) (Const v)
          binds = map pairToBinding $ Map.toList r
      in displayRecord binds
    ExternalRecord ft fp ->
      display $
        case ft of
          YamlFile ->
            -- TODO FilePath Value
            FunctionCallE (Name loadYamlFunctionName) (List.fromList [Const (String (Text.pack fp))])
          MarkdownFile ->
            FunctionCallE (Name loadMarkdownFunctionName) (List.fromList [Const (String (Text.pack fp))])
          TemplateFile _delims binds ->
            FunctionCallE (Name loadTemplateFunctionName) (List.fromList [Const (String (Text.pack fp)), Const (Record binds)])

instance Display ValueType where
  display = \case
    NumberT -> "number"
    TextT -> "text"
    BooleanT -> "boolean"
    ArrayT -> "array"
    RecordT -> "record"
    OutputT -> "output"

displayList :: (a -> Doc Markup) -> List a -> Doc Markup
displayList disp lst =
  brackets $
    align . sep . punctuate comma $
    toList . List.map disp $ lst

displayRecord :: Display1 f => [RecordBinding f] -> Doc Markup
displayRecord binds =
  nest 2 $ lbrace <+> (align $ sep $ punctuate comma (map display binds)) <+> rbrace

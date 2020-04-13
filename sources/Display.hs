{-|
Description : Pretty-printing of abstract syntax trees.

This module defines the 'Display' class for pretty-printing, and instances for
template abstract syntax trees and their component types.
-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wmissing-exported-signatures #-}
module Display
where

import Prelude hiding (group)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import Development.Shake
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)
import Text.MMark (MMarkErr)
import qualified Text.Show as Sh

import KnownFunction
import qualified List
import List (List)
import Eval.Problem
import Syntax
import Value

instance Display Problem where
  display Problem{ problemWhere, problemDescription } =
      nest 2 $
        vsep
          [ "•" <+> display problemDescription
          , nest 2 $ "In this expression:" <> line <> liftedDisplay problemWhere ]

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
    ProblemInsufficientArguments ins -> display ins
    ProblemArgumentTypeMismatches aes -> display aes
    ProblemNameNotFound name namesInScope ->
      errorMessage ("Name not found: " <> display name) $
        "‘" <> display name <> "’" <+> "is not available here;" <> softline
        <> nest 2 ("these names are available:" <> softline
        <> fillSep (punctuate comma (map display namesInScope)))
    ProblemUnknownFunction name knownFuncs ->
      errorMessage ("Unknown function:" <+> display name) $
        nest 2 ("These functions are available:" <> line
        <> fillSep (punctuate comma (map display knownFuncs)))
    ProblemFieldNotFound field others ->
      errorMessage ("Field not found: " <> display field) $
        nest 2 ("These fields exist in this record:" <> softline
        <> fillSep (punctuate comma (map display others)))

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

instance Display InsufficientArguments where
  display (InsufficientArguments got) =
    errorMessage "Wrong number of arguments!" $
      pretty got <+> "were given" <> comma <> softline
      <> "but at least one additional argument was expected."

instance Display ArgumentTypeMismatches where
  display (ArgumentTypeMismatches errors) =
    vsep . map dispArgErr . IntMap.toList $ errors
    where
      dispArgErr = \(n, (TypeMismatch val types)) ->
        nest 2 $ "The" <+> ordinal n <+> "argument," <> softline
          <> nest 2 ("namely" <> softline <> display val <> comma) <> softline
          <> "is a" <+> display (valueType val) <> comma <> softline
          <> nest 2 (expectedTypes types)
      expectedTypes types =
        case toList types of
          [] ->
            error "no types!" -- TODO use NonEmpty
          ty1:tys ->
            "but was expected to be a" <+> display ty1
              <> foldMap ((("," <> softline <> "or a ") <>) . display) tys
      ordinal n = case n of
        1 -> "1st"; 2 -> "2nd"; 3 -> "3rd"
        4 -> "4th"; 5 -> "5th"; 6 -> "6th"
        7 -> "7th"; 8 -> "8th"; 9 -> "9th"
        10 -> "10th"
        _ -> error ("weird number of arguments: " <> show n)

errorMessage heading body =
  nest 2 $ annotate Heading heading <> line <> body

instance Display ShakeException where
  display ShakeException{..} =
      vsep (map fineagle shakeExceptionStack) <> line <> inner
    where
      inner
        | Just (EvalError problems) <- fromException shakeExceptionInner
          = vsep (punctuate line (map display problems))
        | Just (ParseError str) <- fromException shakeExceptionInner
          = pretty str <> line
        | Just (NotAnObject _ft _fp) <- fromException shakeExceptionInner
          = "not an object!"
        | otherwise
          = pretty (displayException shakeExceptionInner) <> line
      fineagle str =
        case str of
          '*':' ':rest ->
            pretty '•' <+> pretty rest
          ' ':' ':rest ->
            indent 2 (pretty rest)
          _ ->
            pretty str

module Syntax.Display
  ( displayList
  , displayFieldAccess, displayFieldAccessLineBreak
  , displayFunctionCall, displayFunctionCallLineBreak
  , Spacing(..), LineBreak(..)
  )
where

import Data.Text.Prettyprint.Doc

import List

displayFieldAccessLineBreak ::
  -- | What kind of line break to put between the expression and the field.
  LineBreak ->
  -- | The expression.
  Doc any ->
  -- | The field name.
  Doc any ->
  -- | The optionality modifier.
  Doc any ->
  Doc any
displayFieldAccessLineBreak br inner name opt =
  nest 2 $ inner <> renderLineBreak br <> dot <> name <> opt

displayFieldAccess :: Doc any -> Doc any -> Doc any -> Doc any
displayFieldAccess = displayFieldAccessLineBreak (May Tight)

renderLineBreak :: LineBreak -> Doc any
renderLineBreak = \case
  Must -> hardline
  May Loose -> softline
  May Tight -> softline'
  Should Loose -> line
  Should Tight -> line'

data Spacing = Loose | Tight

data LineBreak
  = Must
  | May Spacing
  | Should Spacing

displayFunctionCall :: Doc any -> [Doc any] -> Doc any
displayFunctionCall name args =
  displayFunctionCallLineBreak (May Tight) name args

displayFunctionCallLineBreak :: LineBreak -> Doc any -> [Doc any] -> Doc any
displayFunctionCallLineBreak br name args =
  nest 2 $ name <> renderLineBreak br <>
    ( encloseSep (lparen <> space) (space <> renderLineBreak br <> rparen) (comma <> space) args )

displayList :: (a -> Doc any) -> List a -> Doc any
displayList disp lst =
  align $
    encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) $
      toList (List.map disp lst)

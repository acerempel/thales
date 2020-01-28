{-# LANGUAGE StrictData #-}
module Syntax where

import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Scientific
import Text.Show

import Value

{- TODO: This ADT will have to have to have all constructors tagged
with 'SourcePos' for error reporting. -}
data Statement
  = EmptyS
  | VerbatimS Verbatim
  | ExprS Expr
  | ForS Name Expr [Statement]
  | Optional Expr
  | Optionally Expr [Statement]
  deriving Show

data ExprF a
  = LiteralE Literal
  | ArrayE [a]
  | ApplyE a a
  | FieldAccessE Name a
  | NameE Name

{- TODO: Make this 'Show' instance behave like a derived one, and add a
'display' family of functions for displaying expressions and statements
like they appear in source templates. Those functions should return
some kind of terminal-prettyprinting datatype so we can bold stuff.
-}
instance Show1 ExprF where
  liftShowsPrec showsPrecA showsListA prec = \case
    LiteralE l ->
      showsPrec prec l
    ArrayE a ->
      showsListA a
    ApplyE f z ->
      ('(' :)
      . showsPrecA prec f
      . (' ' :)
      . showsPrecA prec z
      . (')' :)
    FieldAccessE n a ->
      ('(' :)
      . showsPrecA prec a
      . (')' :)
      . ('.' :)
      . showsPrec prec n
    NameE n ->
      showsPrec prec n

instance Show a => Show (ExprF a) where
  showsPrec prec = showsPrec1 prec

type Expr = Fix ExprF

data Literal
  = NumberL Scientific
  | StringL Text
  | BooleanL Bool
  deriving Show

literalToValue :: Literal -> Value f
literalToValue = \case
  NumberL  n -> Number n
  StringL  s -> String s
  BooleanL b -> Boolean b

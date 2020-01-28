{-# LANGUAGE StrictData #-}
module Syntax where

import Data.Scientific
import Text.Show

import Value

{- TODO: This ADT will have to have to have all constructors tagged
with 'SourcePos' for error reporting. -}
{- TODO: Replace 'Expr' with a type variable, so that it can be replaced
with the result of evaluating the expression.-}
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
  deriving Show

newtype Expr = Expr { getExpr :: ExprF Expr }

-- | Just wrap the derived 'Show' instance for
-- 'ExprF', but don't show the outer 'Expr' constructor, for brevity.
instance Show Expr where
  showsPrec prec (Expr e) =
    showsPrec prec e

-- TODO: some kind of nice 'display' family of functions for Expr
-- and Statement. Should return 'Doc' from some kind of pretty-printing
-- library.

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

{-# LANGUAGE StrictData #-}
module Syntax where

import Data.Scientific

import Value

{- TODO: This ADT will have to have to have all constructors tagged
with 'SourcePos' for error reporting. -}
{- TODO: Replace 'Expr' with a type variable, so that it can be replaced
with the result of evaluating the expression.-}
data Statement
  = VerbatimS Verbatim
  | ExprS Expr
  | ForS Name Expr [Statement]
  | Optional Expr
  | Optionally Expr [Statement]
  deriving ( Show, Eq )

data ExprF a
  = LiteralE Literal
  | ArrayE [a]
  | ApplyE a a
  | FieldAccessE Name a
  | NameE Name
  deriving ( Show, Eq )

newtype Expr = Expr
  { getExpr :: ExprF Expr }
  deriving newtype ( Show, Eq )

-- TODO: some kind of nice 'display' family of functions for Expr
-- and Statement. Should return 'Doc' from some kind of pretty-printing
-- library.

data Literal
  = NumberL Scientific
  | StringL Text
  | BooleanL Bool
  deriving ( Show, Eq )

literalToValue :: Literal -> Value f
literalToValue = \case
  NumberL  n -> Number n
  StringL  s -> String s
  BooleanL b -> Boolean b

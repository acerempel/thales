{-# LANGUAGE StrictData #-}
module Syntax where

import Value

data Syntax
  = VerbatimS Verbatim
  | StatementS Statement

data PartialStatement
  = BlockS ([Syntax] -> Statement)
  | StandaloneS Statement

data Statement
  = EmptyS
  | ExprS Expr
  | ForS Name Expr [Syntax]
  | Optional Expr
  | Optionally Expr [Syntax]

data Expr
  = LiteralE Data
  | ApplyE Expr Expr
  | NameE Name

type Name = Text

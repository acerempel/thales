{-# LANGUAGE StrictData #-}
module Syntax where

import Value

data Syntax
  = VerbatimS Verbatim
  | StatementS Statement
  deriving Show

data PartialStatement
  = BlockS ([Syntax] -> Statement)
  | StandaloneS Statement

data Statement
  = EmptyS
  | ExprS Expr
  | ForS Name Expr [Syntax]
  | Optional Expr
  | Optionally Expr [Syntax]
  deriving Show

data Expr
  = LiteralE Data
  | ApplyE Expr Expr
  | NameE Name
  deriving Show

type Name = Text

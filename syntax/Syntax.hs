{-# LANGUAGE StrictData #-}
module Syntax where

import Data.Functor.Classes
import Data.Scientific
import Text.Megaparsec
import Text.Show

import Verbatim

type Name = Text

{- TODO: This ADT will have to have to have all constructors tagged
with 'SourcePos' for error reporting. -}
{- TODO: Replace 'Expr' with a type variable, so that it can be replaced
with the result of evaluating the expression.-}
data Statement
  = VerbatimS Verbatim
  | ExprS SourcePos Expr
  | ForS SourcePos Name Expr [Statement]
  | Optional SourcePos Expr
  | Optionally SourcePos Name Expr [Statement]
  deriving ( Show, Eq )

data ExprH f
  = LiteralE Literal
  | ArrayE ([f (ExprH f)])
  | ApplyE (f (ExprH f)) (f (ExprH f))
  | FieldAccessE Name (f (ExprH f))
  | NameE Name

instance Show1 f => Show (ExprH f) where
  -- TODO: use prec correctly!
  showsPrec prec = \case
    LiteralE lit ->
      showsPrec prec lit
    ArrayE arr ->
      liftShowsPrec
      (liftShowsPrec showsPrec showList)
      (liftShowList showsPrec showList)
      prec arr
    ApplyE fe ae ->
      ("ApplyE (" <>)
      . liftShowsPrec showsPrec showList prec fe
      . (") (" <>)
      . liftShowsPrec showsPrec showList prec ae
      . (')' :)
    FieldAccessE n e ->
      ("FieldAccessE " <>)
      . showsPrec prec n
      . (" (" <>)
      . liftShowsPrec showsPrec showList prec e
      . (')' :)
    NameE n ->
      showsPrec prec n

instance Eq1 f => Eq (ExprH f) where
  (LiteralE l1) == (LiteralE l2) =
    l1 == l2
  (ArrayE v1) == (ArrayE v2) =
    liftEq (liftEq (==)) v1 v2
  (ApplyE fe1 ae1) == (ApplyE fe2 ae2) =
    liftEq (==) fe1 ae1 && liftEq (==) fe2 ae2
  (FieldAccessE n1 e1) == (FieldAccessE n2 e2) =
    n1 == n2 && liftEq (==) e1 e2
  (NameE n1) == (NameE n2) =
    n1 == n2
  _ == _ =
    False

newtype Id a = Id
  { getId :: a }
  deriving newtype ( Show, Eq )

instance Show1 Id where
  liftShowsPrec showsPrecA _showsListA prec (Id a) =
    showsPrecA prec a

instance Eq1 Id where
  liftEq eqA (Id a) (Id b) =
    eqA a b

type Expr = ExprH Id

data Literal
  = NumberL Scientific
  | StringL Text
  | BooleanL Bool
  deriving ( Show, Eq )

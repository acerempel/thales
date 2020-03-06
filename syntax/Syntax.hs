{-|
Description : The definition of the abstract syntax tree.

I dunno, this is pretty self-explanatory. The only interesting thing is 'ExprH' –
see the "Eval.Expr" module for how the parameter to that type is used.
-}
module Syntax where

import Data.Functor.Classes
import Data.Scientific
import Text.Megaparsec
import Text.Show

import NonEmptyText
import List (List)
import Verbatim

-- | A name, to which a value may be bound. This is the sort of thing that is
-- usually called a variable, except that these names are strictly immutable –
-- they simply refer to values.
--
-- (Currently there is no way to bind a value to a name within a template -- it
-- must already exist in the context in which the template is evaluated. This is
-- a TODO.)
newtype Name = Name { fromName :: NonEmptyText }
  deriving newtype ( Eq, Ord, Show, Hashable, NFData, IsString )

{- TODO: Replace 'Expr' with a type variable, so that it can be replaced
with the result of evaluating the expression.-}
-- | The things that can occur at the top level of a template. They are all
-- expected to produce some string output somehow.
data Statement
  -- | A piece of verbatim text, to appear in the same place in the output.
  = VerbatimS Verbatim
  -- | An expression, which should be evaluated, the result expected to be a
  -- 'Text', which is then spliced into the output.
  | ExprS SourcePos Expr
  -- | Iterate over the element in the 'Expr', which is expected to evaluate to
  -- a 'List', and in each iteration bind the element to 'Name', and evaluate
  -- the '[Statement]' in that context.
  | ForS SourcePos Name Expr [Statement]
  -- | Evaluate the expression; if an error occurs -- e.g. if a variable is not
  -- found -- then simply output nothing, rather than propogate the error.
  | Optional SourcePos Expr
  -- | Like 'Optional', but if evaluating the expression is successful, bind the
  -- result to the 'Name', and evaluate the '[Statement]'s in that context.
  | Optionally SourcePos Name Expr [Statement]
  deriving ( Show, Eq )

-- | An expression. The expression language is quite limited at the moment, but
-- I imagine I will expand it a bit. The H in 'ExprH' stands for "higher-order"
-- -- in reference to the @f@ type parameter, which has the kind @'Type' ->
-- Type@. It is used to wrap recursive uses of 'ExprH', with the purpose of
-- allowing flexibility in representing different states of the syntax tree. 
-- See "Eval.Expr" and "Eval" for an example of its use.
data ExprH f
  -- | A literal thing of data, like @10.2@.
  = LiteralE Literal
  -- | An array of expressions, like @[1, "seven", [2]]@.
  | ArrayE (List (f (ExprH f)))
  -- | A field access, like @post.description@.
  | FieldAccessE Name (f (ExprH f))
  -- | A bare name, like @potato@.
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
  (FieldAccessE n1 e1) == (FieldAccessE n2 e2) =
    n1 == n2 && liftEq (==) e1 e2
  (NameE n1) == (NameE n2) =
    n1 == n2
  _ == _ =
    False

-- | 'Id' is short for "Identity". This is like
-- 'Data.Functor.Identity.Identity', but I redefined it here for some reason,
-- possibly for the 'Show' instance, or possibly because the name is shorter.
newtype Id a = Id
  { getId :: a }
  deriving newtype ( Show, Eq )

-- | Note that the "Id" constructor is /not/ shown!
instance Show1 Id where
  liftShowsPrec showsPrecA _showsListA prec (Id a) =
    showsPrecA prec a

instance Eq1 Id where
  liftEq eqA (Id a) (Id b) =
    eqA a b

-- | An 'ExprH Id', i.e., an 'Expr' whose constructors contain no extra
-- information.
type Expr = ExprH Id

-- | A piece of literal scalar data -- cannot contain other expressions, simple,
-- atomic.
data Literal
  = NumberL Scientific
  | StringL Text
  | BooleanL Bool
  deriving ( Show, Eq )

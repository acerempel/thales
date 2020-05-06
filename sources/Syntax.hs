{-# LANGUAGE UndecidableInstances #-}
{-|
Description : The definition of the abstract syntax tree.

I dunno, this is pretty self-explanatory. The only interesting thing is 'ExprH' –
see the "Eval.Expr" module for how the parameter to that type is used.
-}
module Syntax
  ( Name(..)
  , Statement(..)
  , ExprF(..) , Expr, Rec(..)
  , RecordBinding(..)
  , Literal(..)
  , IsOptional(..), displayOptionality
  , SourcePos(..)
  , Delimiters(..), defaultDelimiters
  , displayExpr, displayExprF
  , displayRecordBinding
  , displayRecord
  , displayLiteral, displayName
  )
where

import Prelude hiding (group)

import Data.Scientific
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal (unsafeTextWithoutNewlines)
import Development.Shake.Classes
import Text.Megaparsec

import List (List)
import Syntax.Display

-- | A name, to which a value may be bound. This is the sort of thing that is
-- usually called a variable, except that these names are strictly immutable –
-- they simply refer to values.
--
-- (Currently there is no way to bind a value to a name within a template -- it
-- must already exist in the context in which the template is evaluated. This is
-- a TODO.)
newtype Name = Name { fromName :: Text }
  deriving newtype ( Eq, Ord, Show, Hashable, Binary, NFData, IsString )

displayName :: Name -> Doc any
displayName (Name n) = unsafeTextWithoutNewlines n

{- TODO: Replace 'Expr' with a type variable, so that it can be replaced
with the result of evaluating the expression.-}
-- | The things that can occur at the top level of a template. They are all
-- expected to produce some string output somehow.
data Statement
  -- | A piece of verbatim text, to appear in the same place in the output.
  -- The 'Char' is just the first character of the verbatim text.
  = VerbatimS Char Text
  -- | An expression, which should be evaluated, the result expected to be a
  -- 'Text', which is then spliced into the output.
  | ExprS SourcePos Expr
  -- | Iterate over the element in the 'Expr', which is expected to evaluate to
  -- a 'List', and in each iteration bind the element to 'Name', and evaluate
  -- the '[Statement]' in that context.
  | ForS SourcePos Name Expr [Statement]
  -- | If evaluating the expression is successful, bind the
  -- result to the 'Name', and evaluate the '[Statement]'s in that context.
  | OptionallyS SourcePos Expr (Maybe Name) [Statement]
  -- | Evaluate some expressions, bind the result of each to the respective
  -- 'Name', and evaluate the 'Statement's with those names in scope.
  | LetInS SourcePos [(Name, Expr)] [Statement]
  | LetS SourcePos [(Name, Expr)]
  -- | Evaluate the given bindings and provide them as part of the result of
  -- executing the template, as an associative array.
  | ExportS SourcePos [RecordBinding (Rec ExprF)]
  -- | Insert the body of a document from another file – e.g., a template, a
  -- markdown file – as this point. The 'Expr' represents the document – it must be a 'LoadedDoc'.
  | IncludeBodyS SourcePos Expr
  deriving ( Eq, Show )

-- | An expression. The expression language is quite limited at the moment, but
-- I imagine I will expand it a bit. The H in 'ExprH' stands for "higher-order"
-- -- in reference to the @f@ type parameter, which has the kind @'Type' ->
-- Type@. It is used to wrap recursive uses of 'ExprH', with the purpose of
-- allowing flexibility in representing different states of the syntax tree. 
-- See "Eval.Expr" and "Eval" for an example of its use.
data ExprF a
  -- | A literal thing of data, like @10.2@.
  = LiteralE Literal
  -- | An array of expressions, like @[1, "seven", [2]]@.
  | ArrayE (List a)
  | RecordE [RecordBinding a]
  -- | A field access, like @post.description@.
  | FieldAccessE Name IsOptional a
  -- | A bare name, like @potato@.
  | NameE Name
  | FunctionCallE Name [a]
  deriving ( Generic, Eq, Show, Functor )

newtype IsOptional = IsOptional Bool
  deriving ( Show, Eq )

displayOptionality :: IsOptional -> Doc any
displayOptionality (IsOptional b) =
  if b then pretty '?' else mempty

-- | The sort of binding that may occur in a record literal – also used in the
-- @export@ statement.
data RecordBinding a
  -- | E.g. @{ thingy }@ – short for @{ thingy = thingy }@.
  = FieldPun Name
  -- | E.g. @{ foo = [1, 2, 3] }@.
  | FieldAssignment Name a
  deriving ( Generic, Eq, Show, Functor )

displayRecordBinding :: (a -> Doc any) -> RecordBinding a -> Doc any
displayRecordBinding displayInner recBind =
  case recBind of
    FieldPun n ->
      displayName n
    FieldAssignment n expr ->
      nest 2 $ displayName n <+> equals <> softline <> displayInner expr

{-| The strings that delimit bits of code, or directives, or whatever
you want to call them, in a template. E.g. @Delimiters "{{" "}}"@,
or @Delimiters { begin = "$(", end = ")" }@. -}
data Delimiters = Delimiters
  { begin :: Text
  , end :: Text }
  deriving stock ( Show, Eq, Generic, Typeable )
  deriving anyclass ( Hashable, NFData, Binary )

{-| A default set of delimiters – @{{ ... }}@, same as what Mustache templates
use (and Jinja2 and Liquid, for expression splices). -}
defaultDelimiters :: Delimiters
defaultDelimiters = Delimiters "{{" "}}"

-- | An 'ExprH Id', i.e., an 'Expr' whose constructors contain no extra
-- information.
type Expr = ExprF (Rec ExprF)

newtype Rec f = Rec { unRec :: f (Rec f) }

deriving instance (forall a. Show a => Show (f a)) => Show (Rec f)
deriving instance (forall a. Eq a => Eq (f a)) => Eq (Rec f)

displayExprF :: (a -> Doc any) -> ExprF a -> Doc any
displayExprF displayInner expr =
  case expr of
    LiteralE lit ->
      displayLiteral lit
    ArrayE vec ->
      displayList displayInner vec
    FieldAccessE n i a ->
      displayFieldAccess (displayInner a) (displayName n) (displayOptionality i)
    NameE n ->
      displayName n
    RecordE binds ->
      displayRecord displayInner binds
    FunctionCallE name args ->
      displayFunctionCall (displayName name) (map displayInner args)

displayExpr :: Expr -> Doc any
displayExpr = displayExprF (displayExpr . unRec)

-- | A piece of literal scalar data -- cannot contain other expressions, simple,
-- atomic.
data Literal
  = NumberL Scientific
  | StringL Text
  | BooleanL Bool
  | EmptyL
  deriving ( Show, Eq, Generic )

displayLiteral :: Literal -> Doc anything
displayLiteral = \case
  NumberL n -> unsafeViaShow n
  StringL s -> viaShow s
  BooleanL b -> pretty b
  EmptyL -> "empty"

displayRecord :: (a -> Doc any) -> [RecordBinding a] -> Doc any
displayRecord displayInner binds =
  nest 2 $
    lbrace
    <+> (align $ sep $ punctuate comma $
      map (displayRecordBinding displayInner) binds)
    <+> rbrace

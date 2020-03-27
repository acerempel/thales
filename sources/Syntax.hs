{-# LANGUAGE UndecidableInstances #-}
{-|
Description : The definition of the abstract syntax tree.

I dunno, this is pretty self-explanatory. The only interesting thing is 'ExprH' –
see the "Eval.Expr" module for how the parameter to that type is used.
-}
module Syntax
  ( Name(..)
  , Statement(..)
  , ExprH(..) , Expr, Id(..)
  , RecordBinding(..)
  , Literal(..)
  , SourcePos(..)
  , FileType(..)
  , Delimiters(..), defaultDelimiters
  )
where

import Data.Functor.Classes
import Data.Scientific
import Development.Shake.Classes
import Text.Megaparsec

import NonEmptyText
import List (List)

-- | A name, to which a value may be bound. This is the sort of thing that is
-- usually called a variable, except that these names are strictly immutable –
-- they simply refer to values.
--
-- (Currently there is no way to bind a value to a name within a template -- it
-- must already exist in the context in which the template is evaluated. This is
-- a TODO.)
newtype Name = Name { fromName :: Text }
  deriving newtype ( Eq, Ord, Show, Hashable, NFData, IsString )

{- TODO: Replace 'Expr' with a type variable, so that it can be replaced
with the result of evaluating the expression.-}
-- | The things that can occur at the top level of a template. They are all
-- expected to produce some string output somehow.
data Statement
  -- | A piece of verbatim text, to appear in the same place in the output.
  = VerbatimS NonEmptyText
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
  | LetS SourcePos [(Name, Expr)] [Statement]
  -- | Evaluate the given bindings and provide them as part of the result of
  -- executing the template, as an associative array.
  | ExportS SourcePos [RecordBinding Id]
  deriving ( Eq, Show )

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
  | RecordE [RecordBinding f]
  -- | A field access, like @post.description@.
  | FieldAccessE Name (f (ExprH f))
  -- | A bare name, like @potato@.
  | NameE Name
  | FunctionCallE Name (List (f (ExprH f)))
  deriving ( Generic )

deriving instance (forall a. Show a => Show (f a)) => Show (ExprH f)
deriving instance (forall a. Eq a => Eq (f a)) => Eq (ExprH f)

-- | The sort of binding that may occur in a record literal – also used in the
-- @export@ statement.
data RecordBinding f
  -- | E.g. @{ thingy }@ – short for @{ thingy = thingy }@.
  = FieldPun Name
  -- | E.g. @{ foo = [1, 2, 3] }@.
  | FieldAssignment Name (f (ExprH f))
  deriving ( Generic )

deriving instance (forall a. Show a => Show (f a)) => Show (RecordBinding f)
deriving instance (forall a. Eq a => Eq (f a)) => Eq (RecordBinding f)

-- | A type of file that may be interpreted as a key-value mapping, i.e. a
-- 'Record'.
data FileType a
  -- | The YAML file is assumed to have an associative array at the top level
  -- with string keys.
  = YamlFile
  -- | Any YAML front matter is treated as with 'YamlFile', and the document
  -- body is available under the "body" key.
  | MarkdownFile
  -- | The output of executing the template is available under the "body" key.
  -- The argument to this constructor represents the parameters given to the
  -- template. In the abstract syntax tree ('Syntax'), this is an 'ExprH',
  -- and in a 'Value', this is a @'HashMap' 'Text' 'Value'@. The template will
  -- be parsed with the provided 'Delimiters'.
  | TemplateFile Delimiters a
  deriving stock ( Eq, Show, Generic, Functor )
  deriving anyclass ( Hashable, NFData, Typeable, Binary )

{-| The strings that delimit bits of code, or directives, or whatever
you want to call them, in a template. E.g. @Delimiters "{{" "}}"@,
or @Delimiters { begin = "$(", end = ")" }@. -}
data Delimiters = Delimiters
  { begin :: NonEmptyText
  , end :: NonEmptyText }
  deriving stock ( Show, Eq, Generic, Typeable )
  deriving anyclass ( Hashable, NFData, Binary )

{-| A default set of delimiters – @{{ ... }}@, same as what Mustache templates
use (and Jinja2 and Liquid, for expression splices). -}
defaultDelimiters :: Delimiters
defaultDelimiters = Delimiters "{{" "}}"

instance Foldable FileType where
  foldMap f (TemplateFile _d a) = f a
  foldMap _f _ = mempty

instance Traversable FileType where
  traverse f (TemplateFile d a) = TemplateFile d <$> f a
  traverse _f YamlFile = pure YamlFile
  traverse _f MarkdownFile = pure MarkdownFile

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
  deriving ( Show, Eq, Generic )

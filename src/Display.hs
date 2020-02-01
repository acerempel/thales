module Display where

import Prelude hiding (group)

import Data.Text.Prettyprint.Doc
import qualified Data.Vector as Vec

import Syntax

data Markup = Important | Emphasized

data Precedence = Tight | Loose

class Display a where
  display :: Precedence -> a -> Doc Markup

instance Display a => Display (ExprF a) where
  display prec = \case
    LiteralE lit -> display prec lit
    ArrayE vec ->
          lbracket
      <+> (align . group . vsep . punctuate comma)
          ((Vec.toList . Vec.map (display Loose)) vec)
      <+> rbracket
    ApplyE f a ->
      let parenthesizeMaybe =
            case prec of
              Tight -> parens
              Loose -> id
      in parenthesizeMaybe $ sep [ display Loose f, display Tight a ]
    FieldAccessE n a ->
      group $ display Tight a <> line' <> dot <> pretty n
    NameE n ->
      pretty n

instance Display Expr where
  display prec (Expr expr) = display prec expr

instance (Display a, Display b) => Display (Either a b) where
  display prec = either (display prec) (display prec)

instance Display Literal where
  display _prec = \case
    NumberL n -> unsafeViaShow n
    StringL s -> pretty s
    BooleanL b -> unsafeViaShow b

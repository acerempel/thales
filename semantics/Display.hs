module Display where

import Prelude hiding (group)

import Data.Text.Prettyprint.Doc
import qualified Data.Vector as Vec

import Syntax

data Markup = Important | Emphasized

data Precedence = Tight | Loose

class Display a where
  display :: Precedence -> a -> Doc Markup
  default display :: (a ~ h f, DisplayH h, Display1 f) => Precedence -> a -> Doc Markup
  display = hoistDisplay

instance Display1 f => Display (ExprH f)

class Display1 (f :: Type -> Type) where
  liftDisplay :: (Precedence -> a -> Doc Markup) -> Precedence -> f a -> Doc Markup

class DisplayH (h :: (Type -> Type) -> Type) where
  hoistDisplay :: Display1 f => Precedence -> h f -> Doc Markup

instance DisplayH ExprH where
  hoistDisplay prec = \case
    LiteralE lit ->
      display prec lit
    ArrayE vec ->
          lbracket
      <+> (align . group . vsep . punctuate comma)
          ((Vec.toList . Vec.map (liftDisplay display Loose)) vec)
      <+> rbracket
    ApplyE f a ->
      let parenthesizeMaybe =
            case prec of Tight -> parens; Loose -> id
      in parenthesizeMaybe $
          sep [ liftDisplay display Loose f, liftDisplay display Tight a ]
    FieldAccessE n a ->
      group $ liftDisplay display Tight a <> line' <> dot <> pretty n
    NameE n ->
      pretty n

instance Display1 Id where
  liftDisplay displayA prec (Id a) =
    displayA prec a

instance Display Literal where
  display _prec = \case
    NumberL n -> unsafeViaShow n
    StringL s -> pretty s
    BooleanL b -> unsafeViaShow b

module Display
  ( Display(..), Display1(..), DisplayH(..)
  , Markup(..)
  )
where

import Prelude hiding (group)

import Data.Text.Prettyprint.Doc

import qualified List
import Syntax

data Markup = Important | Emphasized

class Display a where
  display :: a -> Doc Markup
  default display :: (a ~ h f, DisplayH h, Display1 f) => a -> Doc Markup
  display = hoistDisplay

instance Display1 f => Display (ExprH f)
instance Display1 f => Display (RecordBinding f)

class Display1 (f :: Type -> Type) where
  liftedDisplay :: Display a => f a -> Doc Markup

class DisplayH (h :: (Type -> Type) -> Type) where
  hoistDisplay :: Display1 f => h f -> Doc Markup

instance DisplayH ExprH where
  hoistDisplay = \case
    LiteralE lit ->
      display lit
    ArrayE vec ->
      brackets $
        align . sep . punctuate comma $
        toList . List.map liftedDisplay $ vec
    FieldAccessE n a ->
      group $ liftedDisplay a <> line' <> dot <> display n
    NameE n ->
      display n
    RecordE binds ->
      lbrace <+> (nest 2 $ sep $ punctuate comma (map display binds)) <+> rbrace
    FunctionCallE name args ->
      cat [ display name
          , nest 2 $ parens $
            align . sep . punctuate comma $
            toList . List.map liftedDisplay $ args
          ]

instance DisplayH RecordBinding where
  hoistDisplay = \case
    FieldPun n ->
      display n
    FieldAssignment n expr ->
      display n <+> equals <+> softline <+> nest 2 (liftedDisplay expr)

instance Display1 Id where
  liftedDisplay (Id a) =
    display a

instance Display Literal where
  display = \case
    NumberL n -> unsafeViaShow n
    StringL s -> viaShow s
    BooleanL b -> unsafeViaShow b

instance Display Name where
  display (Name net) =
    unsafeViaShow net

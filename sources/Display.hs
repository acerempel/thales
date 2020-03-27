module Display
  ( Display(..), Display1(..), DisplayH(..)
  , Markup(..), Precedence(..)
  )
where

import Prelude hiding (group)

import Data.Text.Prettyprint.Doc

import qualified List
import Syntax

data Markup = Important | Emphasized

data Precedence = Tight | Loose

class Display a where
  display :: Precedence -> a -> Doc Markup
  default display :: (a ~ h f, DisplayH h, Display1 f) => Precedence -> a -> Doc Markup
  display = hoistDisplay

instance Display1 f => Display (ExprH f)
instance Display1 f => Display (RecordBinding f)

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
          ((toList . List.map (liftDisplay display Loose)) vec)
      <+> rbracket
    FieldAccessE n a ->
      group $ liftDisplay display Tight a <> line' <> dot <> display prec n
    NameE n ->
      display prec n
    RecordE binds ->
      lbrace <+> (nest 2 $ sep $ punctuate comma (map (display prec) binds)) <+> rbrace
    ListDirectoryE expr ->
      enclosePrec prec $
        sep ["list-directory", nest 2 $ liftDisplay display Tight expr]
    FileE ft expr ->
      enclosePrec prec $
        sep ["load-file-TODO", nest 2 $ liftDisplay display Tight expr]

enclosePrec :: Precedence -> Doc ann -> Doc ann
enclosePrec = \case
  Loose -> id
  Tight -> parens

instance DisplayH RecordBinding where
  hoistDisplay _prec = \case
    FieldPun n ->
      display Loose n
    FieldAssignment n expr ->
      display Loose n <+> equals <+> softline <+> nest 2 (liftDisplay display Loose expr)

instance Display1 Id where
  liftDisplay displayA prec (Id a) =
    displayA prec a

instance Display Literal where
  display _prec = \case
    NumberL n -> unsafeViaShow n
    StringL s -> viaShow s
    BooleanL b -> unsafeViaShow b

instance Display Name where
  display _prec (Name net) =
    unsafeViaShow net

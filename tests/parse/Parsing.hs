{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Main (main) where

import Data.Scientific
import qualified Data.Text as Text
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Vector as Vec
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck
import Text.Megaparsec (errorBundlePretty)

import Display
import NonEmptyText (NonEmptyText(..))
import Parse
import Syntax

renderExpr =
  renderStrict
  . layoutPretty (LayoutOptions (AvailablePerLine 80 0.75))
  . display Loose

parseExpr =
  runParser exprP defaultDelimiters "tests/Parsing.hs"

parseDisplayedExpr (Generated expr) =
  let rendered = renderExpr expr
  in bimap errorBundlePretty (const (Text.unpack rendered)) $
      parseExpr rendered

main =
  defaultMain $
    testProperty "parse/display roundtrip" parseDisplayedExpr

newtype GeneratedExpr = Generated Expr
  deriving newtype ( Show )

instance Monad m => Serial m GeneratedExpr where
  series = Generated <$> genericSeries

instance Serial m a => Serial m (Vec.Vector a) where
  series =
    cons0 Vec.empty \/
    cons1 Vec.singleton \/
    cons2 (\a b -> Vec.fromList [a, b]) \/
    cons3 (\a b c -> Vec.fromList [a, b, c]) \/
    cons4 (\a b c d -> Vec.fromList [a, b, c, d])

instance Serial m a => Serial m (Id a) where
  series = newtypeCons Id

instance Monad m => Serial m Name where
  series = newtypeCons Name

instance Monad m => Serial m Text.Text where
  series = Text.pack <$> series

instance Monad m => Serial m Literal

instance Monad m => Serial m Scientific where
  series = cons2 scientific

instance (Monad m, (forall a. Serial m a => Serial m (f a))) => Serial m (ExprH f)
instance (Monad m, (forall a. Serial m a => Serial m (f a))) => Serial m (RecordBinding f)
instance (Monad m, Serial m a) => Serial m (FileType a)

instance Monad m => Serial m Delimiters where
  series = cons2 Delimiters

instance Monad m => Serial m NonEmptyText where
  series = cons2 NonEmptyText

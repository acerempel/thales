{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
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

import Parse
import Syntax

-- | Pretty-print an expression.
renderExpr =
  renderStrict
  . layoutPretty (LayoutOptions (AvailablePerLine 80 0.75))
  . displayExpr

parseExpr =
  runParser exprP defaultDelimiters "tests/Parsing.hs"

parseDisplayedExpr (expr :: Expr) =
  let rendered = renderExpr expr
  in bimap errorBundlePretty (const (Text.unpack rendered)) $
      parseExpr rendered

main =
  -- If the smallcheck depth is 5 or more, the tests take longer to terminate
  -- than I wanted to wait, viz., longer than 20 seconds! At depth 4, it runs
  -- 460 tests, which I think is enough.
  defaultMain $ localOption (SmallCheckDepth 4) $
    testProperty "parse/display roundtrip" parseDisplayedExpr

-- | Only generates 'Vector's from 0 through 4 elements in length.
instance Serial m a => Serial m (Vec.Vector a) where
  series =
    cons0 Vec.empty \/
    cons1 Vec.singleton \/
    cons2 (\a b -> Vec.fromList [a, b]) \/
    cons3 (\a b c -> Vec.fromList [a, b, c]) \/
    cons4 (\a b c d -> Vec.fromList [a, b, c, d])

instance Monad m => Serial m Name where
  -- We never parse an empty Name, so make sure we never generate one!
  series = newtypeCons (Name . Text.pack . getNonEmpty)

instance Monad m => Serial m Text.Text where
  series = Text.pack <$> series

instance Monad m => Serial m Literal

instance Monad m => Serial m Scientific where
  series = cons2 scientific

instance (Monad m, Serial m a) => Serial m (ExprF a)
instance (Monad m, Serial m a) => Serial m (RecordBinding a)
instance (Monad m, forall a. Serial m a => Serial m (f a)) => Serial m (Rec f) where
  series = newtypeCons Rec

instance Monad m => Serial m Delimiters where
  series = cons2 Delimiters

{-# LANGUAGE DeriveFunctor #-}
module Value where

import Data.Functor.Classes
import Data.Functor.Foldable
import qualified Data.HashMap.Strict as Map
import Data.Scientific
import Data.Text.Lazy.Builder as Text
import Data.Vector
import qualified Data.Vector as Vec
import Text.Show

data Value f
  = Verbatim !Verbatim
  | Data (Value f)
  -- | A function in @f@, where @f@ needs to be a 'Monad' in order to be
  -- useful. N.B. that there is no syntax for creating a function!
  | Function !(Value f -> f (Value f))

data DataF a
  = Number !Scientific
  | String !Text
  | Boolean !Bool
  | Record !(HashMap Text a)
  | Array !(Vector a)
  deriving ( Functor, Eq )

instance Show1 DataF where
  liftShowsPrec showsPrecA showListA prec = \case
    Number  s -> showsPrec prec s
    String  t -> showsPrec prec t
    Boolean b -> showsPrec prec b
    Record  h -> Map.foldrWithKey (\k v s -> showsPrec prec k . (':':) . showsPrecA prec v . s) id h
    Array   a -> Vec.foldr (\v s -> showsPrecA prec v . (',':) . s) id a

type Data = Fix DataF

type Verbatim = Text.Builder

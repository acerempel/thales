{-# LANGUAGE DeriveFunctor #-}
module Value where

import Data.Functor.Foldable
import Data.Scientific
import Data.Text.Lazy.Builder as Text
import Data.Vector

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

type Data = Fix DataF

type Verbatim = Text.Builder

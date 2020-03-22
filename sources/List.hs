{-# OPTIONS_GHC -Wno-missing-signatures -Wno-orphans #-}
module List
  ( List
  , Vec.empty, Vec.singleton, Vec.map
  , concat, Vec.concatMap, Vec.fromList
  , update, unsafeUpdate, imapA )
where

import Prelude hiding (concat)
import qualified Data.Vector as Vec

type List = Vec.Vector

update i a v = v Vec.// [(i,a)]
unsafeUpdate i a v = Vec.unsafeUpd v [(i,a)]

imapA :: Monad f => (Int -> a -> f b) -> Vec.Vector a -> f (Vec.Vector b)
imapA = Vec.imapM

concat = Vec.concat . Vec.toList

instance Hashable a => Hashable (Vec.Vector a) where
  hashWithSalt salt vec =
    hashWithSalt salt (Vec.toList vec)

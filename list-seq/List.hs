{-# OPTIONS_GHC -Wno-missing-signatures #-}
module List
  ( List
  , Seq.update, unsafeUpdate
  , Seq.singleton, Seq.empty
  , Seq.fromList
  , concat, concatMap
  , map, imapA )
where

import Prelude hiding (concat, concatMap, map)
import Data.Sequence as Seq

type List = Seq

unsafeUpdate = update

imapA :: Monad m => (Int -> a -> m b) -> Seq a -> m (Seq b)
imapA = traverseWithIndex

map :: (a -> b) -> Seq a -> Seq b
map = fmap

concatMap :: (a -> Seq b) -> Seq a -> Seq b
concatMap f s = s >>= f

concat :: Seq (Seq a) -> Seq a
concat = foldl' (<>) Seq.empty

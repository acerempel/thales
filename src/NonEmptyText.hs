{-# OPTIONS_GHC -Wno-missing-signatures #-}
module NonEmptyText
  ( NonEmptyText, nonEmptyText, fromNonEmptyText
  , head, tail )
where

import Prelude hiding (head, tail)
import qualified Data.Text as Text

newtype NonEmptyText =
  NonEmptyText { fromNonEmptyText :: Text }
  deriving newtype ( Eq, Ord, Semigroup, Monoid, IsString, Show )

nonEmptyText :: Text -> Maybe NonEmptyText
nonEmptyText t =
  if Text.length t < 1
    then Nothing
    else Just (NonEmptyText t)

head = Text.head . fromNonEmptyText

tail = Text.tail . fromNonEmptyText

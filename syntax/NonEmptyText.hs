{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE BangPatterns #-}
module NonEmptyText
  ( NonEmptyText, nonEmptyText, fromNonEmptyText
  , head, tail )
where

import Prelude hiding (head, tail)
import qualified Data.Text as Text

{-| This is simply 'Text', but it cannot be empty! -}
newtype NonEmptyText =
  NonEmptyText { fromNonEmptyText :: Text }
  deriving newtype ( Eq, Ord, Semigroup, Monoid, Show, Hashable, NFData )

nonEmptyText :: Text -> Maybe NonEmptyText
nonEmptyText !t =
  if Text.null t
    then Nothing
    else Just (NonEmptyText t)

instance IsString NonEmptyText where
  -- | Calls 'error' if the given 'String' is empty.
  fromString str =
    case str of
      [] ->
        error "NonEmptyText.fromString: given String is empty!"
      _ ->
        NonEmptyText (fromString str)

head = Text.head . fromNonEmptyText

tail = Text.tail . fromNonEmptyText

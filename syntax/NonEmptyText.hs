{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE BangPatterns #-}
{-|
Description : Provides a 'NonEmptyText' datatype for 'Text' that cannot be empty.
-}
module NonEmptyText
  ( NonEmptyText, fromText, toText
  , head, tail, foldr, foldl', length )
where

import Prelude hiding (head, tail, foldl', foldr, toText, length)
import qualified Data.Text as Text

{-| This is simply 'Text', but it cannot be empty! -}
newtype NonEmptyText =
  NonEmptyText { toText :: Text }
  deriving newtype ( Eq, Ord, Semigroup, Monoid, Show, Hashable, NFData )

fromText :: Text -> Maybe NonEmptyText
fromText !t =
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

head = Text.head . toText

tail = Text.tail . toText

foldr f a net = Text.foldr f a (toText net)
foldl' f a net = Text.foldl' f a (toText net)
length = Text.length . toText

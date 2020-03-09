{-# OPTIONS_GHC -Wno-missing-signatures -funbox-strict-fields #-}
{-|
Description : Provides a 'NonEmptyText' datatype for 'Text' that cannot be empty.
-}
module NonEmptyText
  ( NonEmptyText(..), singleton, fromText, toText
  , head, tail, foldr, foldl', length )
where

import Prelude hiding (head, tail, foldl', foldr, toText, length)
import qualified Data.Text as Text

{-| This is simply 'Text', but it cannot be empty! -}
data NonEmptyText =
  NonEmptyText Char Text
  deriving stock ( Eq, Ord, Show, Generic )
  deriving anyclass ( Hashable, NFData )

singleton :: Char -> NonEmptyText
singleton c = NonEmptyText c Text.empty

fromText :: Text -> Maybe NonEmptyText
fromText t =
  uncurry NonEmptyText <$> Text.uncons t

toText :: NonEmptyText -> Text
toText (NonEmptyText hd tl) =
  Text.cons hd tl

instance IsString NonEmptyText where
  -- | Calls 'error' if the given 'String' is empty.
  fromString str =
    case str of
      [] ->
        error "NonEmptyText.fromString: given String is empty!"
      hd:tl ->
        NonEmptyText hd (fromString tl)

head (NonEmptyText hd _) = hd

tail (NonEmptyText _ tl) = tl

foldr f a (NonEmptyText hd tl) =
  f hd (Text.foldr f a tl)

foldl' f a (NonEmptyText hd tl) =
  Text.foldl' (f a hd) a tl

length (NonEmptyText _ tl) =
  1 + Text.length tl

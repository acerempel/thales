{-# LANGUAGE StrictData #-}
module Value ( Value(..) ) where

import qualified Data.HashMap.Strict as Map
import Data.Scientific
import Text.Show

import List (List)
import Syntax
import Verbatim

{-| A 'Value' is a thing that may be the value of a name in a template.
Effectively, templates are dynamically typed. Aside from the absence of null,
this is basically equivalent to Aeson's @Value@
type.-}
data Value where
  Number :: Scientific -> Value
  String :: Text -> Value
  Verbatim :: Verbatim -> Value
  Boolean :: Bool -> Value
  Array :: List Value -> Value
  Record :: HashMap Name Value -> Value

instance Eq Value where
  (Number a1) == (Number a2) =
    a1 == a2
  (String a1) == (String a2) =
    a1 == a2
  (Verbatim a1) == (Verbatim a2) =
    a1 == a2
  (Array a1) == (Array a2) =
    a1 == a2
  (Record a1) == (Record a2) =
    a1 == a2
  _ == _ =
    False

instance Show Value where
  showsPrec prec = \case
    Number  s -> showsPrec prec s
    String  t -> showsPrec prec t
    Boolean b -> showsPrec prec b
    Record  h ->
        ('{' :)
      . Map.foldrWithKey
        (\k v s -> showsPrec prec k . (':':) . showsPrec prec v . s) id h
      . ('}' :)
    Array   a -> showList (toList a)
    Verbatim _v -> ("..." <>)

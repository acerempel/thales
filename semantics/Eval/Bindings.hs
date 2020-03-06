module Eval.Bindings where

import qualified Data.HashMap.Strict as Map

import Syntax
import Value

-- | A 'Bindings' provides the bindings that are available at the top level
-- in a template. At the moment it is just a type synonym, but this will
-- probably change.
newtype Bindings = Bindings
  { getBindings :: HashMap Name Value }
  deriving newtype ( Show, Monoid, Semigroup )

union :: Bindings -> Bindings -> Bindings
union (Bindings a) (Bindings b) =
  Bindings (a `Map.union` b)

insert :: Name -> Value -> Bindings -> Bindings
insert n v (Bindings binds) =
  Bindings (Map.insert n v binds)

fromList :: [(Name, Value)] -> Bindings
fromList = coerce . Map.fromList

singleton :: Name -> Value -> Bindings
singleton n v = Bindings (Map.singleton n v)

empty :: Bindings
empty = Bindings Map.empty

class HasLocalBindings m where

  addLocalBindings :: [(Name, Value)] -> m a -> m a

  addLocalBinding :: Name -> Value -> m a -> m a
  addLocalBinding n v =
    addLocalBindings [(n, v)]

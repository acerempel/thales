module Eval.Bindings where

import Syntax
import Value

-- | A 'Bindings' provides the bindings that are available at the top level
-- in a template. At the moment it is just a type synonym, but this will
-- probably change.
type Bindings = HashMap Name Value

class HasLocalBindings m where

  addLocalBindings :: [(Name, Value)] -> m a -> m a

  addLocalBinding :: Name -> Value -> m a -> m a
  addLocalBinding n v =
    addLocalBindings [(n, v)]

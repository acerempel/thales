module Function where

import BaseMonad
import Value

data Function where
  Function :: ValueType a -> (a -> M Value) -> Function

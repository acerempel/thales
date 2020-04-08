module Bindings
  ( Bindings
  , Map.fromList, Map.union, Map.lookup
  , Map.insert, Map.empty, Map.singleton
  )
where

import qualified Data.HashMap.Strict as Map

import Value

type Bindings = HashMap Text Value

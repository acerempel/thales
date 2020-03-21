{-# OPTIONS_GHC -Wno-missing-methods #-}
module DependencyMonad where

import Bindings
import Output
import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType -> FilePath -> Text -> m (Maybe Value)

  execTemplate :: Bindings -> FilePath -> m (Bindings, Output)

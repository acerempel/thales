{-# OPTIONS_GHC -Wno-missing-methods #-}
module DependencyMonad where

import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType -> FilePath -> Text -> m (Maybe Value)

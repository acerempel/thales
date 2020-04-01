module DependencyMonad ( DependencyMonad(..) ) where

import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType -> FilePath -> Text -> m (Maybe Value)

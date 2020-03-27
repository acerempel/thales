module DependencyMonad where

import Bindings
import Syntax (FileType)
import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType Bindings -> FilePath -> Text -> m (Maybe Value)

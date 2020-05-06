module DependencyMonad.Class ( DependencyMonad(..) ) where

import Output (Output)
import Syntax (Name)
import Value

class (Monad m, MonadIO m) => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType -> FilePath -> Text -> m (Maybe Value)

  listFields :: FileType -> FilePath -> m [Name]

  getBody :: FileType -> FilePath -> m Output

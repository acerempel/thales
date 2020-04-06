module DependencyMonad ( DependencyMonad(..) ) where

import Value
import Syntax (Name)

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupField :: FileType -> FilePath -> Text -> m (Maybe Value)

  listFields :: FileType -> FilePath -> m [Name]

module DependencyMonad where

import qualified System.Directory as System

import Value

class Monad m => DependencyMonad m where

  listDirectory :: FilePath -> m [FilePath]

  lookupYaml :: FilePath -> Text -> m Value

instance DependencyMonad IO where
  listDirectory dir = sort <$> System.listDirectory dir
  lookupYaml fp t = fail "zrop"

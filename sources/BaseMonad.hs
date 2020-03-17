module BaseMonad where

import qualified System.Directory as System

import Value

class Monad m => BaseMonad m where

  listDirectory :: FilePath -> m [FilePath]

  loadFile :: (FilePath -> m Value) -> FilePath -> m Value

instance BaseMonad IO where
  listDirectory dir = sort <$> System.listDirectory dir
  {-# INLINE listDirectory #-}
  loadFile = id
  {-# INLINE loadFile #-}

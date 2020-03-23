{-# OPTIONS_GHC -Wno-missing-methods #-}
module DependencyMonad where

import Development.Shake.Classes

data FileType
  = YamlFile
  | MarkdownFile

instance Eq FileType
instance Show FileType
instance Generic FileType
instance Hashable FileType
instance NFData FileType
instance Binary FileType

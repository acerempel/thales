module Shake where

import Development.Shake

import DependencyMonad

instance DependencyMonad Action where
  listDirectory = getDirectoryContents
  loadFile = id -- TODO
  run = id -- TODO

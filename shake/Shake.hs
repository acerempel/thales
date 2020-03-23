module Shake where

import Development.Shake

import BaseMonad

instance BaseMonad Action where
  listDirectory = getDirectoryContents
  loadFile = id -- TODO
  run = id -- TODO

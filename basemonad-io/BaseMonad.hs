{-# OPTIONS_GHC -Wno-missing-signatures #-}
module BaseMonad where

import qualified System.Directory as System

import Value

type M = IO

listDirectory = System.listDirectory

loadFile :: (FilePath -> IO Value) -> FilePath -> IO Value
loadFile = id

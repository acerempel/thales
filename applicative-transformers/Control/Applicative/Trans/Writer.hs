{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Writer.Strict
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The strict 'WriterT' monad transformer, which adds collection of
-- outputs (such as a count or string output) to a given monad.
--
-- This monad transformer provides only limited access to the output
-- during the computation.  For more general access, use
-- "Control.Monad.Trans.State" instead.
--
-- This version builds its output strictly; for a lazy version with
-- the same interface, see "Control.Monad.Trans.Writer.Lazy".
-- Although the output is built strictly, it is not possible to
-- achieve constant space behaviour with this transformer: for that,
-- use "Control.Monad.Trans.Writer.CPS" instead.
-----------------------------------------------------------------------------

module Control.Applicative.Trans.Writer (
    -- * The Writer applicative functor
    Writer,
    writer,
    runWriter,
    execWriter,
    mapWriter,
    -- * The WriterT applicative transformer
    WriterT(..),
    execWriterT,
    mapWriterT,
    -- * Writer operations
    tell,
  ) where

import Data.Functor.Identity

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Writer.Strict (WriterT(..), Writer)
import Prelude hiding (null, length)

-- | Construct a writer computation from a (result, output) pair.
-- (The inverse of 'runWriter'.)
writer :: (Applicative m) => (a, w) -> WriterT w m a
writer = WriterT . pure
{-# INLINE writer #-}

-- | Unwrap a writer computation as a (result, output) pair.
-- (The inverse of 'writer'.)
runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT
{-# INLINE runWriter #-}

-- | Extract the output from a writer computation.
--
-- * @'execWriter' m = 'snd' ('runWriter' m)@
execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)
{-# INLINE execWriter #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriter' ('mapWriter' f m) = f ('runWriter' m)@
mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = mapWriterT (Identity . f . runIdentity)
{-# INLINE mapWriter #-}

-- | Extract the output from a writer computation.
--
-- * @'execWriterT' m = 'liftM' 'snd' ('runWriterT' m)@
execWriterT :: (Functor m) => WriterT w m a -> m w
execWriterT m =
    snd <$> runWriterT m
{-# INLINE execWriterT #-}

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriterT' ('mapWriterT' f m) = f ('runWriterT' m)@
mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)
{-# INLINE mapWriterT #-}

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Applicative m) => w -> WriterT w m ()
tell w = writer ((), w)
{-# INLINE tell #-}

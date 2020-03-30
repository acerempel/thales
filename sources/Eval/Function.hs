module Eval.Function
  ( FunctionM
  , textArgument, arrayArgument
  , withOneArgument, withTwoArguments
  , ArgumentErrors(..)
  )
where

import Prelude hiding (withReaderT)

import Control.Applicative.Trans.Reader
import Control.Applicative.Trans.Validation
import qualified Control.Applicative.Trans.Validation as Validation
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.IntMap.Strict as IntMap
import Data.Tuple.Only

import List (List)
import Value

type FunctionM args errors result = ReaderT args (Validation errors) result

data ArgumentError = ArgumentError Value (DList ValueType)

-- | Note that this instance assumes the 'Value' is the same!
instance Semigroup ArgumentError where
  (ArgumentError val types1) <> (ArgumentError _ types2) =
    ArgumentError val (types1 <> types2)

textArgument :: FunctionM Value (DList ValueType) Text
textArgument = ReaderT $ \val ->
  case val of
    String text ->
      success text
    _ ->
      failure (DList.singleton TextT)

arrayArgument :: FunctionM Value (DList ValueType) (List Value)
arrayArgument = ReaderT $ \val ->
  case val of
    Array arr ->
      success arr
    _ ->
      failure (DList.singleton ArrayT)

withOneArgument :: FunctionM argument errors result -> FunctionM (Only argument) (ArgumentErrors errors) result
withOneArgument =
  withReaderT fromOnly . mapReaderT (Validation.mapFailures (argumentNumber 1))

withTwoArguments ::
  Semigroup errors =>
  (a -> b -> c) ->
  FunctionM argument errors a ->
  FunctionM argument errors b ->
  FunctionM (argument, argument) (ArgumentErrors errors) c
withTwoArguments f r1 r2 =
  ReaderT $ \(a1, a2) ->
    liftA2 f
      (Validation.mapFailures (argumentNumber 1) (runReaderT r1 a1))
      (Validation.mapFailures (argumentNumber 2) (runReaderT r2 a2))

argumentNumber :: Int -> errors -> ArgumentErrors errors
argumentNumber n e =
  ArgumentErrors (IntMap.singleton n e)

newtype ArgumentErrors = ArgumentErrors { fromArgumentErrors :: IntMap ArgumentError }

instance Semigroup ArgumentErrors where
  a1 <> a2 = ArgumentErrors $
    IntMap.unionWith (<>) (fromArgumentErrors a1) (fromArgumentErrors a2)

instance Monoid ArgumentErrors where
  mempty = ArgumentErrors IntMap.empty

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
import Problem
import Value

type FunctionM args errors result = ReaderT args (Validation errors) result

textArgument :: FunctionM Value TypeMismatch Text
textArgument = ReaderT $ \val ->
  case val of
    String text ->
      success text
    _ ->
      failure (TypeMismatch val (DList.singleton TextT))

arrayArgument :: FunctionM Value TypeMismatch (List Value)
arrayArgument = ReaderT $ \val ->
  case val of
    Array arr ->
      success arr
    _ ->
      failure (TypeMismatch val (DList.singleton ArrayT))

withOneArgument :: FunctionM argument TypeMismatch result -> FunctionM (Only argument) ArgumentErrors result
withOneArgument =
  withReaderT fromOnly . mapReaderT (Validation.mapFailures (argumentNumber 1))

withTwoArguments ::
  (a -> b -> c) ->
  FunctionM argument TypeMismatch a ->
  FunctionM argument TypeMismatch b ->
  FunctionM (argument, argument) ArgumentErrors c
withTwoArguments f r1 r2 =
  ReaderT $ \(a1, a2) ->
    liftA2 f
      (Validation.mapFailures (argumentNumber 1) (runReaderT r1 a1))
      (Validation.mapFailures (argumentNumber 2) (runReaderT r2 a2))

argumentNumber :: Int -> TypeMismatch -> ArgumentErrors
argumentNumber n e =
  ArgumentErrors (IntMap.singleton n e)

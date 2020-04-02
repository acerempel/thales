module Eval.Function
  ( FunctionM
  , textArgument, arrayArgument
  , liftF1, liftF2
  , withOneArgument, withTwoArguments
  , evalFunctionM
  )
where

import Prelude hiding (withReaderT)

import Control.Applicative.Trans.Reader
import Control.Applicative.Trans.Validation
import qualified Control.Applicative.Trans.Validation as Validation
import qualified Data.DList as DList
import qualified Data.IntMap.Strict as IntMap
import Data.Tuple.Only

import Eval.Expr
import List (List)
import Problem
import Value

newtype FunctionM args errors result = FunctionM
  { runFunctionM :: ReaderT args (Validation errors) result }
  deriving newtype (Functor, Applicative, Monad, Alternative)

textArgument :: FunctionM Value TypeMismatch Text
textArgument = FunctionM $ ReaderT $ \val ->
  case val of
    String text ->
      success text
    _ ->
      failure (TypeMismatch val (DList.singleton TextT))

arrayArgument :: FunctionM Value TypeMismatch (List Value)
arrayArgument = FunctionM $ ReaderT $ \val ->
  case val of
    Array arr ->
      success arr
    _ ->
      failure (TypeMismatch val (DList.singleton ArrayT))

liftF1 :: FunctionM argument TypeMismatch result -> FunctionM (Only argument) ArgumentErrors result
liftF1 =
  FunctionM
  . withReaderT fromOnly
  . mapReaderT (Validation.mapFailures (argumentNumber 1))
  . runFunctionM

liftF2 ::
  (a -> b -> c) ->
  FunctionM argument TypeMismatch a ->
  FunctionM argument TypeMismatch b ->
  FunctionM (argument, argument) ArgumentErrors c
liftF2 f r1 r2 = FunctionM $
  ReaderT $ \(a1, a2) ->
    liftA2 f
      (Validation.mapFailures (argumentNumber 1) (runReaderT (runFunctionM r1) a1))
      (Validation.mapFailures (argumentNumber 2) (runReaderT (runFunctionM r2) a2))

argumentNumber :: Int -> TypeMismatch -> ArgumentErrors
argumentNumber n e =
  ArgumentErrors (IntMap.singleton n e)

withOneArgument ::
  FunctionM (Only arg) ArgumentErrors result ->
  FunctionM [arg] ProblemDescription result
withOneArgument (FunctionM (ReaderT func)) = FunctionM $ ReaderT $ \args ->
  case args of
    [arg1] ->
      Validation.mapFailures ProblemArgumentErrors $ func (Only arg1)
    _ ->
      failure (ProblemWrongNumberOfArguments
        (WrongNumberOfArguments { expected = 1, actual = length args }))

withTwoArguments ::
  FunctionM (arg, arg) ArgumentErrors result ->
  FunctionM [arg] ProblemDescription result
withTwoArguments (FunctionM (ReaderT func)) = FunctionM $ ReaderT $ \args ->
  case args of
    [arg1, arg2] ->
      Validation.mapFailures ProblemArgumentErrors $ func (arg1, arg2)
    _ ->
      failure (ProblemWrongNumberOfArguments
        (WrongNumberOfArguments { expected = 2, actual = length args }))

evalFunctionM ::
  Monad m =>
  FunctionM args ProblemDescription result ->
  args ->
  ExprT m result
evalFunctionM (FunctionM (ReaderT func)) =
  either zutAlors return
  . runIdentity
  . runValidationT
  . func

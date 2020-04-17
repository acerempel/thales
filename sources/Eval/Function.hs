{-| Description : Dynamically-typed template functions

This module implements functions in the template language. Since the template language is
dynamically typed, a function is defined by a 'Signature' that specifies what types of
arguments it takes, in what order, and then you can use 'applySignature' to check an argument
list (i.e., a @['Value']@) against the 'Signature', and if the arguments are of the required
types and so forth, you get the result.
-}
{-# LANGUAGE MultiWayIf #-}
module Eval.Function
  ( Signature
  , argument
  , eitherArgument
  , noMoreArguments
  , restOfArguments
  , applySignature
  )
where

import Prelude hiding (Alt(..), get, gets, put)

import Control.Monad.Trans.State.Strict
import Control.Applicative.Trans.Validation
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap

import Eval.Problem
import Value

{-| I wanted to say that this describes the types and order of arguments
that a function expects, but that would not be quite right. Essentially a 'Signature'
is a /parser/ for an argument list, and the result of a successful parse is the
application of the function. I.e., in a 'Signature a', the result of the function
is @a@. You use this like a parser-combinator library, with the 'Applicative'
and 'Alternative' instances. (Combinators from "Control.Applicative.Combinators" work
here too! It's not a 'Monad' though.) The other key function is 'argument'.
-}
data Signature a where
  Alt :: Signature a -> Signature a -> Signature a
  Arg :: ValueType t -> Signature t
  ArgEither :: ValueType t1 -> ValueType t2 -> Signature (Either t1 t2)
  Fmap :: (a -> b) -> Signature a -> Signature b
  -- The 'Signature' arguments to 'LiftA2' need to be lazy, otherwise 'some'
  -- is an infinite loop!
  LiftA2 :: (a -> b -> c) -> ~(Signature a) -> ~(Signature b) -> Signature c
  Pure :: a -> Signature a
  Fail :: Signature a
  End :: Signature ()

instance Functor Signature where
  fmap = Fmap

instance Applicative Signature where
  pure = Pure
  liftA2 = LiftA2
  f <*> a = LiftA2 ($) f a

{-| In general, order does not matter with this instance – both branches
are explored, and the first branch that succeeds is returned, no matter
what errors other branches return.-}
instance Alternative Signature where
  (<|>) = Alt
  empty = Fail
  some sig = LiftA2 (:) sig (many sig)

{-| Accept an argument of the given type. You can then use 'fmap'
and the 'Applicative' and 'Alternative' combinators to work with
the result.-}
argument :: ValueType t -> Signature t
argument = Arg

eitherArgument :: ValueType t1 -> ValueType t2 -> Signature (Either t1 t2)
eitherArgument = ArgEither

noMoreArguments :: Signature ()
noMoreArguments = End

restOfArguments :: Signature t -> Signature [t]
restOfArguments sig =
  liftA2 (:) sig (restOfArguments sig) <|> [] <$ noMoreArguments

data Failure e
  = CommittedFailure e
  | TentativeFailure e
  | InarticulateFailure
  deriving ( Eq, Show, Functor )

unwrapFailure :: Failure e -> e
unwrapFailure = \case
  CommittedFailure e -> e
  TentativeFailure e -> e
  -- TODO handle this correctly! 'InarticulateFailure' comes only from 'empty'
  -- and 'mzero'.
  InarticulateFailure -> error "Internal error!"

instance Semigroup e => Semigroup (Failure e) where
  CommittedFailure e <> _ = CommittedFailure e
  _ <> CommittedFailure e = CommittedFailure e
  TentativeFailure e1 <> TentativeFailure e2 = TentativeFailure (e1 <> e2)
  InarticulateFailure <> r = r
  l <> InarticulateFailure = l

instance Semigroup e => Monoid (Failure e) where
  mempty = InarticulateFailure

data ArgumentErrors
  = WrongTypes ArgumentTypeMismatches
  | NotEnough InsufficientArguments
  | TooMany WrongNumberOfArguments

instance Semigroup ArgumentErrors where
  WrongTypes tm1 <> WrongTypes tm2 = WrongTypes (tm1 <> tm2)
  e@(WrongTypes _) <> _ = e
  _ <> e@(WrongTypes _) = e
  e@(TooMany _) <> _ = e
  _ <> e@(TooMany _) = e
  a <> _ = a

data SigState = SigState
  { numArgsSeen :: Int
  , nextArgs :: [Value]
  , didConsume :: Bool
  }

applySignature :: Signature a -> [Value] -> Either FunctionCallProblem a
applySignature sig args =
  let validated = runStateT (go sig) (SigState 0 args False)
      result = first unwrapFailure $ runIdentity $ runValidationT validated
  in
    case result of
      Left (WrongTypes errors) ->
        Left (FunctionArgumentTypeMismatches errors)
      Left (NotEnough err) ->
        Left (FunctionInsufficientArguments err)
      Left (TooMany err) ->
        Left (FunctionWrongNumberOfArguments err)
      Right (a, SigState{..}) ->
        case nextArgs of
          (_:_) ->
            Left $
              FunctionWrongNumberOfArguments
              (WrongNumberOfArguments
                { expected = numArgsSeen
                , actual = numArgsSeen + length nextArgs })
          [] ->
            Right a

  where
    -- Type signature needed because of MonoLocalBinds
    go :: Signature b -> StateT SigState (Validation (Failure ArgumentErrors)) b
    go (Arg valt) = do
      requireArg $ \arg ->
        case valueOfType valt arg of
          Nothing -> argumentTypeMismatch [SomeType valt] arg
          Just res -> pure res

    go (ArgEither t1 t2) =
      requireArg $ \arg -> if
        | Just arg1 <- valueOfType t1 arg ->
          pure (Left arg1)
        | Just arg2 <- valueOfType t2 arg ->
          pure (Right arg2)
        | otherwise ->
          argumentTypeMismatch [SomeType t1, SomeType t2] arg

    go (Alt left right) =
      go left <|> go right

    go (LiftA2 f siga sigb) =
      liftA2 f (go siga) (go sigb)

    go (Fmap f siga) =
      f <$> go siga

    go (Pure a) =
      pure a

    go Fail =
      empty

    go End = do
      SigState{..} <- get
      case nextArgs of
        [] -> pure ()
        next -> failed $ TooMany
          WrongNumberOfArguments
            { expected = numArgsSeen, actual = numArgsSeen + length next }

    requireArg action = do
      SigState{..} <- get
      case nextArgs of
        [] -> do
          let err = InsufficientArguments numArgsSeen
          failed $ NotEnough err
        (arg:rest) -> do
          res <- action arg
          put SigState
            { numArgsSeen = numArgsSeen + 1
            , nextArgs = rest
            , didConsume = True }
          pure res

    failed :: ArgumentErrors -> StateT SigState (Validation (Failure ArgumentErrors)) a
    failed err = do
      did_branch_consume <- gets didConsume
      lift $ failure $ if did_branch_consume
        then CommittedFailure err
        else TentativeFailure err

    argumentTypeMismatch :: [SomeValueType] -> Value -> StateT SigState (Validation (Failure ArgumentErrors)) a
    argumentTypeMismatch tys arg = do
      prev_arg <- gets numArgsSeen
      failed $
        WrongTypes (ArgumentTypeMismatches
        (IntMap.singleton (prev_arg + 1)
        (TypeMismatch arg (HashSet.fromList tys))))

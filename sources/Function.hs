{-| Description : Dynamically-typed template functions

This module implements functions in the template language. Since the template language is
dynamically typed, a function is defined by a 'Signature' that specifies what types of
arguments it takes, in what order, and then you can use 'applySignature' to check an argument
list (i.e., a @['Value']@) against the 'Signature', and if the arguments are of the required
types and so forth, you get the result.
-}
module Function
  ( Signature
  , argument
  , applySignature
  )
where

import Prelude hiding (Alt(..), get, gets, put)

import Control.Monad.Trans.State.Strict
import Control.Applicative.Trans.Validation
import qualified Data.DList as DList
import qualified Data.IntMap as IntMap

import Problem
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
  LiftA2 :: (a -> b -> c) -> Signature a -> Signature b -> Signature c
  Pure :: a -> Signature a
  Empty :: Signature a

instance Functor Signature where
  fmap f = go
    where
      go (Alt sig1 sig2) = Alt (fmap f sig1) (fmap f sig2)
      go (LiftA2 g siga sigb) = LiftA2 (\a b -> f (g a b)) siga sigb
      go (Pure a) = Pure (f a)
      go (Arg t) = LiftA2 ($) (Pure f) (Arg t)
      go Empty = Empty

  a <$ _sig =
    Pure a

instance Applicative Signature where
  pure = Pure
  liftA2 = LiftA2
  f <*> a = LiftA2 ($) f a

{-| In general, order does not matter with this instance – both branches
are explored, and the first branch that succeeds is returned, no matter
what errors other branches return.-}
instance Alternative Signature where
  (<|>) = Alt
  empty = Empty

{-| Accept an argument of the given type. You can then use 'fmap'
and the 'Applicative' and 'Alternative' combinators to work with
the result.-}
argument :: ValueType t -> Signature t
argument = Arg

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

newtype ArgumentErrors = ArgErrs (Either ArgumentTypeMismatches InsufficientArguments)

instance Semigroup ArgumentErrors where
  ArgErrs (Left tm1) <> ArgErrs (Left tm2) = ArgErrs (Left (tm1 <> tm2))
  ArgErrs l@(Left _) <> _ = ArgErrs l
  _ <> ArgErrs r@(Left _) = ArgErrs r
  a <> _ = a

data SigState = SigState
  { numArgsSeen :: Int
  , nextArgs :: [Value]
  , didConsume :: Bool
  }

applySignature :: Signature a -> [Value] -> Either ProblemDescription a
applySignature sig args =
  let validated = runStateT (go sig) (SigState 0 args False)
      result = first unwrapFailure $ runIdentity $ runValidationT validated
  in
    case result of
      Left (ArgErrs (Left errors)) ->
        Left (ProblemArgumentTypeMismatches errors)
      Left (ArgErrs (Right err)) ->
        Left (ProblemInsufficientArguments err)
      Right (a, SigState{..}) ->
        case nextArgs of
          (_:_) ->
            Left $
              ProblemWrongNumberOfArguments
              (WrongNumberOfArguments
                { expected = numArgsSeen
                , actual = numArgsSeen + length nextArgs })
          [] ->
            Right a

  where
    -- Type signature needed because of MonoLocalBinds
    go :: Signature b -> StateT SigState (Validation (Failure ArgumentErrors)) b
    go (Arg valt) = do
      SigState{..} <- get
      case nextArgs of
        [] -> do
          let err = InsufficientArguments numArgsSeen
          failed $ ArgErrs (Right err)
        (arg:rest) ->
          case valueOfType valt arg of
            Nothing ->
              failed $ argumentTypeMismatch (numArgsSeen + 1) valt arg
            Just res -> do
              put SigState
                { numArgsSeen = numArgsSeen + 1
                , nextArgs = rest
                , didConsume = True }
              pure res

    go (Alt left right) =
      go left <|> go right

    go (LiftA2 f siga sigb) =
      liftA2 f (go siga) (go sigb)

    go (Pure a) =
      pure a

    go Empty =
      empty

    failed :: ArgumentErrors -> StateT SigState (Validation (Failure ArgumentErrors)) a
    failed err = do
      did_branch_consume <- gets didConsume
      lift $ failure $ if did_branch_consume
        then CommittedFailure err
        else TentativeFailure err

    argumentTypeMismatch :: forall t. Int -> ValueType t -> Value -> ArgumentErrors
    argumentTypeMismatch n valt arg =
      ArgErrs (Left
      (ArgumentTypeMismatches
      (IntMap.singleton n
      (TypeMismatch arg (DList.singleton (SomeType valt))))))

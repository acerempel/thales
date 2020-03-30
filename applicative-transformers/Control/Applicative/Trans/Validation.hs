module Control.Applicative.Trans.Validation
  ( ValidationT(..)
  , Validation
  , mapFailures
  )
where

import Control.Applicative
import Control.Applicative.Trans.Class
import Data.Bifunctor
import Data.Functor.Identity

newtype ValidationT e f a = ValidationT
  { runValidationT :: f (Either e a) }

type Validation e = ValidationT e Identity

instance ApplicativeTrans (ValidationT e) where
  liftApplicative = ValidationT . fmap Right

mapFailures :: Functor f => (e -> e') -> ValidationT e f a -> ValidationT e' f a
mapFailures f (ValidationT v) =
  ValidationT $ fmap (first f) v

instance Functor f => Functor (ValidationT e f) where
  fmap f (ValidationT v) =
    ValidationT $ fmap (fmap f) v

-- TODO: what to do about a Bifunctor instance??

instance (Applicative f, Monoid e) => Applicative (ValidationT e f) where
  vf <*> va =
    ValidationT $ liftA2 apply (runValidationT vf) (runValidationT va)
    where
      apply (Left e1) (Left e2) =
        Left (e1 <> e2)
      apply (Right f) (Right a) =
        Right (f a)
      apply (Left e) (Right _) =
        Left e
      apply (Right _) (Left e) =
        Left e
  pure a =
    ValidationT (pure (Right a))

instance (Applicative f, Monoid e) => Alternative (ValidationT e f) where
  empty =
    ValidationT (pure (Left mempty))
  v1 <|> v2 =
    ValidationT $ liftA2 alt (runValidationT v1) (runValidationT v2)
    where
      alt (Left e1) (Left e2) =
        Left (e1 <> e2)
      alt a1@(Right _) _ =
        a1
      alt _ a2 =
        a2

instance (Monad m, Monoid e) => Monad (ValidationT e m) where
  mva >>= f =
    ValidationT $
      runValidationT mva >>= \va ->
        case va of
          Right a ->
            runValidationT (f a)
          Left e ->
            return (Left e)

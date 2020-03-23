module Control.Applicative.Trans.Class where

class ApplicativeTrans t where
  liftApplicative :: Functor f => f a -> t f a

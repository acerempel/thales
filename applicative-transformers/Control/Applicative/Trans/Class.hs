module Control.Applicative.Trans.Class ( ApplicativeTrans(..) ) where

class ApplicativeTrans t where
  liftApplicative :: Functor f => f a -> t f a

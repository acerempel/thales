module Verbatim
  ( Verbatim, fromVerbatim, verbatimToLText
  , escape, preEscaped, preEscapedSingleton
  )
where

import Data.Text.Lazy.Builder as Builder

newtype Verbatim = Verbatim
  { fromVerbatim :: Builder }
  deriving newtype ( Eq, IsString, Semigroup, Monoid )
  deriving stock Show

escape :: Text -> Verbatim
escape = Verbatim . escape'
  where escape' = Builder.fromText

preEscaped :: Text -> Verbatim
preEscaped = Verbatim . fromText

preEscapedSingleton :: Char -> Verbatim
preEscapedSingleton = Verbatim . Builder.singleton

verbatimToLText :: Verbatim -> LText
verbatimToLText = Builder.toLazyText . fromVerbatim

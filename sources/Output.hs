module Output
  ( Output, toBuilder, write
  , fromText, preEscapedFromText
  , singleton, preEscapedSingleton
  )
where

import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.Encoding as Text

newtype Output = Output
  { fromOutput :: Builder }
  deriving newtype ( IsString, Semigroup, Monoid )

fromText :: Text -> Output
fromText = Output . escape
  where escape = Text.encodeUtf8Builder

preEscapedFromText :: Text -> Output
preEscapedFromText = Output . Text.encodeUtf8Builder

singleton :: Char -> Output
singleton = Output . escape
  where escape = Builder.charUtf8

preEscapedSingleton :: Char -> Output
preEscapedSingleton = Output . Builder.charUtf8

toBuilder :: Output -> Builder
toBuilder = fromOutput

write :: Handle -> Output -> IO ()
write h = Builder.hPutBuilder h . fromOutput

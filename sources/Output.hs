module Output
  ( Output, fromBuilder, toBuilder, write
  , fromText, preEscapedFromText
  , singleton, preEscapedSingleton
  , StorableOutput, toStorable, fromStorable
  )
where

import Data.Binary as Binary
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.Encoding as Text
import Text.Show

newtype Output = Output
  { fromOutput :: Builder }
  deriving newtype ( IsString, Semigroup, Monoid )

fromBuilder :: Builder -> Output
fromBuilder = Output

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

data StorableOutput = StorableOutput
  { stoBuilder :: Output
  , stoByteString :: LByteString }

toStorable :: Output -> StorableOutput
toStorable out =
  StorableOutput out (Builder.toLazyByteString (toBuilder out))

fromStorable :: StorableOutput -> Output
fromStorable = stoBuilder

instance Binary StorableOutput where
  put = Binary.put . stoByteString
  get = fmap createSto Binary.get
    where
      createSto bytes =
        StorableOutput (Output (Builder.lazyByteString bytes)) bytes

instance Hashable StorableOutput where
  hashWithSalt salt sto =
    hashWithSalt salt (stoByteString sto)

instance Show StorableOutput where
  showsPrec prec sto =
    showsPrec prec (stoByteString sto)

instance NFData StorableOutput where
  rnf (StorableOutput {..}) =
    rnf stoByteString `seq` ()

instance Eq StorableOutput where
  (==) = (==) `on` stoByteString

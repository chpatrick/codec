module Data.Binary.Codec
  (
  -- * Binary codecs
    BinaryCodec
  , byteString
  , word8
  -- * Testing
  , reversible
  )
 where

import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import Data.Codec.Codec

type BinaryCodec a = Codec Get PutM a

-- | Get/put an n-byte field.
byteString :: Int -> BinaryCodec BS.ByteString
byteString n = Codec
  { parse = getByteString n
  , produce = \bs -> if BS.length bs == n
      then putByteString bs
      else fail "ByteString wrong size for field."
  }

-- | Get/put a byte.
word8 :: BinaryCodec Word8
word8 = Codec getWord8 putWord8

-- | Verify that a `BinaryCodec` is reversible for a given input.
reversible :: Eq a => BinaryCodec a -> a -> Bool
reversible cd x = case runGetOrFail (parse cd) (runPut (produce cd x)) of
  Left ( _, _, _ ) -> False
  Right ( _, _, y ) -> x == y
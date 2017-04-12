module Data.Binary.Codec
  (
  -- * Binary codecs
    BinaryCodec
  , byteString
  , word8
  , word16be, word16le, word16host
  , word32be, word32le, word32host
  , word64be, word64le, word64host
  , wordhost
  )
 where

import           Control.Monad.Codec
import qualified Data.ByteString as BS
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Word

type BinaryCodec a = Codec Get PutM a

-- | Get/put an n-byte field.
byteString :: Int -> BinaryCodec BS.ByteString
byteString n = Codec
  { codecIn = getByteString n
  , codecOut = \bs -> if BS.length bs == n
      then putByteString bs >> return bs
      else fail "ByteString wrong size for field."
  }

word8 :: BinaryCodec Word8
word8 = voidCodec getWord8 putWord8

word16be :: BinaryCodec Word16
word16be = voidCodec getWord16be putWord16be

word16le :: BinaryCodec Word16
word16le = voidCodec getWord16le putWord16le

word16host :: BinaryCodec Word16
word16host = voidCodec getWord16host putWord16host

word32be :: BinaryCodec Word32
word32be = voidCodec getWord32be putWord32be

word32le :: BinaryCodec Word32
word32le = voidCodec getWord32le putWord32le

word32host :: BinaryCodec Word32
word32host = voidCodec getWord32host putWord32host

word64be :: BinaryCodec Word64
word64be = voidCodec getWord64be putWord64be

word64le :: BinaryCodec Word64
word64le = voidCodec getWord64le putWord64le

word64host :: BinaryCodec Word64
word64host = voidCodec getWord64host putWord64host

wordhost :: BinaryCodec Word
wordhost = voidCodec getWordhost putWordhost

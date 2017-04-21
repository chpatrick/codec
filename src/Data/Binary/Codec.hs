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
  , int8
  , int16be, int16le, int16host
  , int32be, int32le, int32host
  , int64be, int64le, int64host
  , inthost
  )
 where

import           Control.Monad.Codec
import qualified Data.ByteString as BS
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Int
import           Data.Word

type BinaryCodec a = Codec Get PutM a

-- | Get/put an n-byte field.
byteString :: Int -> BinaryCodec BS.ByteString
byteString n = Codec
  { codecIn = getByteString n
  , codecOut = \bs -> if BS.length bs == n
      then bs <$ putByteString bs
      else fail $ "Expected a ByteString of size " ++ show n
  }

word8 :: BinaryCodec Word8
word8 = Codec getWord8 (fmapArg putWord8)

word16be :: BinaryCodec Word16
word16be = Codec getWord16be (fmapArg putWord16be)

word16le :: BinaryCodec Word16
word16le = Codec getWord16le (fmapArg putWord16le)

word16host :: BinaryCodec Word16
word16host = Codec getWord16host (fmapArg putWord16host)

word32be :: BinaryCodec Word32
word32be = Codec getWord32be (fmapArg putWord32be)

word32le :: BinaryCodec Word32
word32le = Codec getWord32le (fmapArg putWord32le)

word32host :: BinaryCodec Word32
word32host = Codec getWord32host (fmapArg putWord32host)

word64be :: BinaryCodec Word64
word64be = Codec getWord64be (fmapArg putWord64be)

word64le :: BinaryCodec Word64
word64le = Codec getWord64le (fmapArg putWord64le)

word64host :: BinaryCodec Word64
word64host = Codec getWord64host (fmapArg putWord64host)

wordhost :: BinaryCodec Word
wordhost = Codec getWordhost (fmapArg putWordhost)

int8 :: BinaryCodec Int8
int8 = Codec getInt8 (fmapArg putInt8)

int16be :: BinaryCodec Int16
int16be = Codec getInt16be (fmapArg putInt16be)

int16le :: BinaryCodec Int16
int16le = Codec getInt16le (fmapArg putInt16le)

int16host :: BinaryCodec Int16
int16host = Codec getInt16host (fmapArg putInt16host)

int32be :: BinaryCodec Int32
int32be = Codec getInt32be (fmapArg putInt32be)

int32le :: BinaryCodec Int32
int32le = Codec getInt32le (fmapArg putInt32le)

int32host :: BinaryCodec Int32
int32host = Codec getInt32host (fmapArg putInt32host)

int64be :: BinaryCodec Int64
int64be = Codec getInt64be (fmapArg putInt64be)

int64le :: BinaryCodec Int64
int64le = Codec getInt64le (fmapArg putInt64le)

int64host :: BinaryCodec Int64
int64host = Codec getInt64host (fmapArg putInt64host)

inthost :: BinaryCodec Int
inthost = Codec getInthost (fmapArg putInthost)

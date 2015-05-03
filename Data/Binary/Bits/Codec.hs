module Data.Binary.Bits.Codec
  ( BitCodec
  , bool
  , word8, word16be, word32be, word64be
  , toBytes
  )
where

import qualified Data.Binary.Bits.Get as G
import Data.Binary.Bits.Put
import qualified Data.Binary.Codec as B

import Data.Codec
import Data.Word

type BitCodec a = Codec G.Block BitPut a

bool :: BitCodec Bool
bool = Codec G.bool putBool

word8 :: Int -> BitCodec Word8
word8 = Codec <$> G.word8 <*> putWord8

word16be :: Int -> BitCodec Word16
word16be = Codec <$> G.word16be <*> putWord16be

word32be :: Int -> BitCodec Word32
word32be = Codec <$> G.word32be <*> putWord32be

word64be :: Int -> BitCodec Word64
word64be = Codec <$> G.word64be <*> putWord64be

-- | Convert a `BitCodec` into a `B.BinaryCodec`.
toBytes :: BitCodec a -> B.BinaryCodec a
toBytes (Codec r w)
  = Codec (G.runBitGet $ G.block r) (runBitPut . w)
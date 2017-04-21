module Data.Binary.Bits.Codec
  ( BitCodec
  , bool
  , word8, word16be, word32be, word64be
  , toBytes
  )
where

import           Control.Monad
import           Control.Monad.Codec
import qualified Data.Binary.Bits.Get as G
import           Data.Binary.Bits.Put
import qualified Data.Binary.Codec as B

import           Data.Word

type BitCodec a = Codec G.Block BitPut a

bool :: BitCodec Bool
bool = Codec G.bool (fmapArg putBool)

bitCodec :: (Int -> G.Block a) -> (Int -> a -> BitPut ()) -> Int -> BitCodec a
bitCodec r w n = Codec (r n) (fmapArg (w n))

word8 :: Int -> BitCodec Word8
word8 = bitCodec G.word8 putWord8

word16be :: Int -> BitCodec Word16
word16be = bitCodec G.word16be putWord16be

word32be :: Int -> BitCodec Word32
word32be = bitCodec G.word32be putWord32be

word64be :: Int -> BitCodec Word64
word64be = bitCodec G.word64be putWord64be

-- | Convert a `BitCodec` into a `B.BinaryCodec`.
toBytes :: BitCodec a -> B.BinaryCodec a
toBytes c = Codec
  { codecIn = G.runBitGet $ G.block $ codecIn c
  , codecOut = fmapArg (runBitPut . void . codecOut c)
  }

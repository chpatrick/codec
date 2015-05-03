module Data.Codec.Examples.Foreign where

#include "time.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign
import Data.Codec
import Foreign.Codec

data TimeSpec = TimeSpec
  { seconds :: Int
  , nanoseconds :: Int
  }

genFields ''TimeSpec

timeSpecCodec :: ForeignCodec TimeSpec
timeSpecCodec = build TimeSpec
  $   f_seconds     >-< field (#offset struct timespec, tv_sec)
  >>> f_nanoseconds >-< field (#offset struct timespec, tv_nsec)

instance Storable TimeSpec where
  sizeOf _ = #{size struct timespec}
  alignment _ = #{alignment struct timespec}
  peek = peekCodec timeSpecCodec
  poke = pokeCodec timeSpecCodec
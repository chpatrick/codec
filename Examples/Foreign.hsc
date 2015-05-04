module Examples.Foreign where

#include "time.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

import Foreign
import Foreign.C
import Data.Codec
import Foreign.Codec

data TM = TM
  { seconds :: Int
  , minutes :: Int
  , hours :: Int
  , monthDay :: Int
  , month :: Int
  , year :: Int
  , weekDay :: Int
  , yearDay :: Int
  , daylightSavingTime :: Bool
  } deriving Show

genFields ''TM

cTimeCodec :: ForeignCodec TM
cTimeCodec = build TM
  $   f_seconds            >-< field (#offset struct tm, tm_sec)   cInt
  >>> f_minutes            >-< field (#offset struct tm, tm_min)   cInt
  >>> f_hours              >-< field (#offset struct tm, tm_hour)  cInt
  >>> f_monthDay           >-< field (#offset struct tm, tm_mday)  cInt
  >>> f_month              >-< field (#offset struct tm, tm_mon)   cInt
  >>> f_year               >-< field (#offset struct tm, tm_year)  cInt
  >>> f_weekDay            >-< field (#offset struct tm, tm_wday)  cInt
  >>> f_yearDay            >-< field (#offset struct tm, tm_yday)  cInt
  >>> f_daylightSavingTime >-< field (#offset struct tm, tm_isdst) cBool

instance Storable TM where
  sizeOf _ = #{size struct tm}
  alignment _ = #{alignment struct tm}
  peek = peekWith cTimeCodec
  poke = pokeWith cTimeCodec

foreign import ccall "time.h strftime" strftime
  :: CString -> CInt -> CString -> Ptr TM -> IO CSize

formatTM :: String -> TM -> IO String
formatTM fmt tm
  = allocaBytes maxSize $ \str -> do
      withCString fmt $ \cfmt ->
        with tm $ \tmp ->
          strftime str (fromIntegral maxSize) cfmt tmp
      peekCString str
  where
    maxSize = 512

testTime :: TM
testTime = TM
  { seconds = 42
  , minutes = 49
  , hours = 13
  , monthDay = 4
  , month = 4
  , year = 115
  , weekDay = 0
  , yearDay = 0
  , daylightSavingTime = False
  }
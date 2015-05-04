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

-- convenience macro that enforces the correct codec type for a field
#define hsc_numField(s, f) \
  hsc_printf("field (%ld) . codecFor (undefined :: ", offsetof(s, f)); \
  hsc_type(typeof(((s*)0)->f)); \
  hsc_printf(")");

cTimeCodec :: ForeignCodec TM
cTimeCodec = build TM
  $   f_seconds            >-< (#numField struct tm, tm_sec)  cast
  >>> f_minutes            >-< (#numField struct tm, tm_min)  cast
  >>> f_hours              >-< (#numField struct tm, tm_hour) cast
  >>> f_monthDay           >-< (#numField struct tm, tm_mday) cast
  >>> f_month              >-< (#numField struct tm, tm_mon)  cast
  >>> f_year               >-< (#numField struct tm, tm_year) cast
  >>> f_weekDay            >-< (#numField struct tm, tm_wday) cast
  >>> f_yearDay            >-< (#numField struct tm, tm_yday) cast
  >>> f_daylightSavingTime >-< (#numField struct tm, tm_yday) cBool

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
      _ <- withCString fmt $ \cfmt ->
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
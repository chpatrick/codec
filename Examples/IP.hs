module Examples.IP where

import Data.Codec
import Data.Binary.Bits.Codec
import Data.Word

data IPv4 = IPv4
  { version :: Word8
  , ihl :: Word8
  , dscp :: Word8
  , ecn :: Word8
  , totalLength :: Word16
  , identification :: Word16
  , flags :: Word8
  , fragmentOffset :: Word16
  , timeToLive :: Word8
  , protocol :: Word8
  , headerChecksum :: Word16
  , sourceIP :: Word32
  , destIP :: Word32
  }

genFields ''IPv4

ipv4Codec :: BitCodec IPv4
ipv4Codec = build IPv4
  $   f_version         >-< word8 4
  >>> f_ihl             >-< word8 4
  >>> f_dscp            >-< word8 6
  >>> f_ecn             >-< word8 2
  >>> f_totalLength     >-< word16be 16
  >>> f_identification  >-< word16be 16
  >>> f_flags           >-< word8 3
  >>> f_fragmentOffset  >-< word16be 13
  >>> f_timeToLive      >-< word8 8
  >>> f_protocol        >-< word8 8
  >>> f_headerChecksum  >-< word16be 16
  >>> f_sourceIP        >-< word32be 32
  >>> f_destIP          >-< word32be 32
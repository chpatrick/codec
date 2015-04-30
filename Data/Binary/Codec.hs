module Data.Binary.Codec
  ( BinaryFieldCodec
  , byteString
  , word8
  )
 where

import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Binary.Put
import Data.Word

import Data.Codec (FieldCodec)

type BinaryFieldCodec a = FieldCodec Get PutM a

-- Data.Binary field codecs
byteString :: Int -> BinaryFieldCodec BS.ByteString
byteString n =
  ( getByteString n
  , \bs -> if BS.length bs == n
      then putByteString bs
      else fail "ByteString wrong size for field."
  )

word8 :: BinaryFieldCodec Word8
word8 = ( getWord8, putWord8 )

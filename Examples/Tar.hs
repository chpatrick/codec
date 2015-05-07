{-# LANGUAGE TemplateHaskell #-}

module Examples.Tar where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Codec
import Data.Word
import Data.Binary.Codec
import Numeric

-- stolen from tar-conduit
data Header = Header {
    headerName :: B.ByteString, -- ^ 100 bytes long
    headerMode :: Word64,
    headerOwnerUID :: Word64,
    headerOwnerGID :: Word64,
    headerFileSize :: Integer, -- ^ 12 bytes
    headerModifyTime :: Integer, -- ^ 12 bytes
    headerChecksum :: Word64,
    headerType :: Word8, -- ^ 1 byte
    headerLinkName :: B.ByteString, -- ^ 100 bytes
    headerMagic :: B.ByteString, -- ^ 6 bytes
    headerVersion :: Word16,
    headerOwnerUserName :: B.ByteString, -- ^ 32 bytes
    headerOwnerGroupName :: B.ByteString, -- ^ 32 bytes
    headerDeviceMajorNumber :: Word64,
    headerDeviceMinorNumber :: Word64,
    headerFilenamePrefix :: B.ByteString -- ^ 155 bytes
    }
    deriving (Eq, Show, Read)

genFields ''Header

-- easy peasy
headerCodec :: BinaryCodec Header
headerCodec =
  Header
    $>> f_headerName              >-< bytes' 100 -- Codec will de/serialize in this order
    >>> f_headerMode              >-< octal 8
    >>> f_headerOwnerUID          >-< octal 8
    >>> f_headerOwnerGID          >-< octal 8
    >>> f_headerFileSize          >-< octal 12
    >>> f_headerModifyTime        >-< octal 12
    >>> f_headerChecksum          >-< octal 8
    >>> f_headerType              >-< word8
    >>> f_headerLinkName          >-< bytes' 100
    >>> f_headerMagic             >-< bytes' 6
    >>> f_headerVersion           >-< octal 2
    >>> f_headerOwnerUserName     >-< bytes' 32
    >>> f_headerOwnerGroupName    >-< bytes' 32
    >>> f_headerDeviceMajorNumber >-< octal 8
    >>> f_headerDeviceMinorNumber >-< octal 8
    >>> f_headerFilenamePrefix    >-< bytes' 155

-- byte field with trailing nulls stripped
bytes' :: Int -> BinaryCodec B.ByteString
bytes' n = mapCodecM trim pad (byteString n)
  where
    trim bs = return (fst $ B.spanEnd (==0) bs)
    pad bs
      | B.length bs <= n = return $ bs `B.append` B.replicate (n - B.length bs) 0
      | otherwise = fail "Serialized ByteString too large for field."

-- zero-padded null/space-terminated ASCII octal
octal :: (Show i, Integral i) => Int -> BinaryCodec i
octal n = mapCodecM parseOct makeOct (byteString n)
  where
    parseOct bs
      | B.null trimmed = return 0
      | otherwise = case readOct (BC.unpack trimmed) of
          [ ( x, _ ) ] -> return x
          _ -> fail $ "Could not parse octal value: " ++ show bs
        where trimmed = BC.takeWhile (`notElem` " \NUL") bs
    makeOct x
      | B.length octBS > n - 1 = fail "Octal value too large for field."
      | otherwise = return $ BC.replicate (n - 1 - B.length octBS) '0' `B.append` octBS `B.snoc` 0
        where octBS = BC.pack $ showOct x ""

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Binary.Codec.Test
  ( binaryTests
  ) where

import           Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Binary.Get (runGetOrFail)
import           Data.Binary.Put (runPutM)
import           Data.Int
import           Data.Word
import           GHC.Generics (Generic)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

import           Control.Monad.Codec
import           Data.Binary.Codec

data RecordA = RecordA
  { recordAInt64 :: Int64
  , recordAWord8 :: Word8
  , recordANestedB :: RecordB
  } deriving (Eq, Ord, Show, Generic)

instance Arbitrary RecordA where
  arbitrary = genericArbitrary
  shrink = genericShrink

data RecordB = RecordB
  { recordBWord16 :: Word16
  , recordBByteString64 :: BS.ByteString
  } deriving (Eq, Ord, Show, Generic)

instance Arbitrary RecordB where
  arbitrary =
    RecordB
      <$> arbitrary
      <*> (BS.pack <$> (replicateM 64 arbitrary))
  shrink (RecordB word bs) =
    RecordB <$> shrink word <*> pure bs

recordACodec :: BinaryCodec RecordA
recordACodec =
  RecordA
    <$> recordAInt64 =. int64le
    <*> recordAWord8 =. word8
    <*> recordANestedB =. recordBCodec

recordBCodec :: BinaryCodec RecordB
recordBCodec =
  RecordB
    <$> recordBWord16 =. word16host
    <*> recordBByteString64 =. byteString 64

binaryRoundTrip :: (Eq a, Show a) => BinaryCodec a -> a -> Property
binaryRoundTrip codec x = Right x === roundTripValue
  where
    roundTripValue = do
      let ( _, encoded ) = runPutM (codecOut codec x)
      ( leftover, _, val ) <- runGetOrFail (codecIn codec) encoded
      unless (LBS.null leftover) $ fail "Codec produced leftover bytes."
      return val

binaryTests :: TestTree
binaryTests = testGroup "Data.Aeson.Codec"
  [ testProperty "Simple" $ binaryRoundTrip recordBCodec
  , testProperty "Nested" $ binaryRoundTrip recordACodec
  ]

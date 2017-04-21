{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Aeson.Codec.Test
  ( aesonTests
  ) where

import           Data.Aeson
import           Data.Aeson.Encoding
import           Data.Aeson.Types
import           GHC.Generics (Generic)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

import           Control.Monad.Codec
import           Data.Aeson.Codec

data RecordA = RecordA
  { recordAInt :: Int
  , recordANestedObj :: RecordB
  , recordANestedArr :: RecordB
  , recordANestedObjs :: [ RecordB ]
  } deriving (Eq, Ord, Show, Generic)

instance Arbitrary RecordA where
  arbitrary = genericArbitrary
  shrink = genericShrink

data RecordB = RecordB
  { recordBString :: String
  , recordBDouble :: Double
  } deriving (Eq, Ord, Show, Generic)

instance Arbitrary RecordB where
  arbitrary = genericArbitrary
  shrink = genericShrink

recordACodec :: JSONCodec RecordA
recordACodec = asObject "RecordA" $
  RecordA
    <$> recordAInt =. field "int"
    <*> recordANestedObj =. field' "nestedObj" recordBObjCodec
    <*> recordANestedArr =. field' "nestedArr" recordBArrCodec
    <*> recordANestedObjs =. field' "nestedObjs" (arrayOf' id id recordBObjCodec)

recordBObjCodec :: JSONCodec RecordB
recordBObjCodec = asObject "RecordB" $
  RecordB
    <$> recordBString =. field "string"
    <*> recordBDouble =. field "double"

recordBArrCodec :: JSONCodec RecordB
recordBArrCodec = asArray "RecordB" $
  RecordB
    <$> recordBString =. element
    <*> recordBDouble =. element

jsonRoundTrip :: (Eq a, Show a) => JSONCodec a -> a -> Property
jsonRoundTrip codec x = Right x === roundTripValue .&&. Right x === roundTripEncoding
  where
    roundTripValue = parseEither (parseJSONCodec codec) (toJSONCodec codec x)
    roundTripEncoding = do
      let bs = encodingToLazyByteString (toEncodingCodec codec x)
      val <- eitherDecode bs
      parseEither (parseJSONCodec codec) val

aesonTests :: TestTree
aesonTests = testGroup "Data.Aeson.Codec"
  [ testProperty "Complex" $ jsonRoundTrip recordACodec
  , testProperty "Object codec" $ jsonRoundTrip recordBObjCodec
  , testProperty "Array codec" $ jsonRoundTrip recordBArrCodec
  ]

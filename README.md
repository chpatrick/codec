# Codec [![Build Status](https://travis-ci.org/chpatrick/codec.svg?branch=master)](https://travis-ci.org/chpatrick/codec) [![Hackage](https://img.shields.io/hackage/v/codec.svg)](http://hackage.haskell.org/package/codec)

Codec makes it simple to write composable bidirectional serializers with a consistent interface.

Just define your data type normally:

```haskell
data RecordB = RecordB
  { recordBString :: String
  , recordBDouble :: Double
  } deriving (Eq, Ord, Show)
```

and then associate each field with a codec using the `=.` operator:

```haskell
recordBObjCodec :: JSONCodec RecordB
recordBObjCodec = asObject "RecordB" $
  RecordB
    <$> recordBString =. field "string"
    <*> recordBDouble =. field "double"
```

That's it! If you want, you can now define `ToJSON` and `FromJSON` instances, or just use it directly:

```haskell
instance ToJSON RecordB where
  toJSON = toJSONCodec recordBObjCodec
  toEncoding = toEncodingCodec recordBObjCodec

instance FromJSON RecordB where
  parseJSON = parseJSONCodec recordBObjCodec
```

Support can be added for almost any serialization library, but `aeson` and `binary` support are included.

JSON example:
```haskell
data RecordA = RecordA
  { recordAInt :: Int
  , recordANestedObj :: RecordB
  , recordANestedArr :: RecordB
  , recordANestedObjs :: [ RecordB ]
  } deriving (Eq, Ord, Show)

data RecordB = RecordB
  { recordBString :: String
  , recordBDouble :: Double
  } deriving (Eq, Ord, Show)

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

-- serialize to array elements
recordBArrCodec :: JSONCodec RecordB
recordBArrCodec = asArray "RecordB" $
  RecordB
    <$> recordBString =. element
    <*> recordBDouble =. element
```

Binary example:
```haskell
data RecordA = RecordA
  { recordAInt64 :: Int64
  , recordAWord8 :: Word8
  , recordANestedB :: RecordB
  } deriving (Eq, Ord, Show)

data RecordB = RecordB
  { recordBWord16 :: Word16
  , recordBByteString64 :: BS.ByteString
  } deriving (Eq, Ord, Show)

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
```

A `Codec` is just a combination of a deserializer `r a`, and a serializer `c -> w a`.
```haskell
data CodecFor r w c a = Codec
  { codecIn :: r a
  , codecOut :: c -> w a
  }
  
type Codec r w a = CodecFor r w a a
```
With `binary` for example, `r` is `Get` and `w` is `PutM`. The reason we have an extra parameter `c` is so that we can associate a `Codec` with a particular field using the `=.` operator:

`(=.) :: (c' -> c) -> CodecFor r w c a -> CodecFor r w c' a`

`Codec` is an instance of `Functor`, `Applicative`, `Monad` and `Profunctor`. You can serialize in any order you like, regardless of field order in the data type:

```haskell
recordBCodecFlipped :: BinaryCodec RecordB
recordBCodecFlipped = do
  bs64 <- recordBByteString64 =. byteString 64
  RecordB
    <$> recordBWord16 =. word16host
    <*> pure bs64
```

### Contributors

`=.` operator and `Profunctor` approach thanks to [Xia Li-yao](https://github.com/lysxia)

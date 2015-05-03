module Data.Codec.Testing where

import Data.Codec.Codec

class ParseResult f where
  toEither :: f a -> Either String a

instance ParseResult (Either String) where
  toEither = id

instance ParseResult Maybe where
  toEither = maybe (Left "Nothing") Right

-- | Verify whether a given codec round-trips a given value successfully.
reversible :: (Eq a, ParseResult f) => ConcreteCodec c f a -> a -> Bool
reversible cd x
  = toEither (parseVal cd $ produceVal cd x) == Right x
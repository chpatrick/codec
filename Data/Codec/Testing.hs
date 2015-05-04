module Data.Codec.Testing
  ( -- * Testing
    ParseResult(..)
  , roundTrip, roundTripStorable
  )
where

import Data.Aeson.Types (Result(..))
import Data.Codec.Codec
import Foreign

class ParseResult f where
  toEither :: f a -> Either String a

instance ParseResult (Either String) where
  toEither = id

instance ParseResult Maybe where
  toEither = maybe (Left "Nothing") Right

instance ParseResult Result where
  toEither = \case
    Error err -> Left err
    Success x -> Right x

-- | Round-trip a value through a `ConcreteCodec` to an `Either String a`.
roundTrip :: ParseResult f => ConcreteCodec c f a -> a -> Either String a
roundTrip cd
  = toEither . parseVal cd . produceVal cd

-- | Round-trip a value through its `Storable` instance.
roundTripStorable :: Storable a => a -> IO a
roundTripStorable x
  = with x peek
module Data.Aeson.Codec
  (
  -- * JSON codecs
     JSONCodec, parseJSONCodec, toJSONCodec
  -- * JSON object codecs
  -- | Object codecs have an `IsString` instance that is equivalent to `entry` `def`.
  , entry, obj
  , ObjectParser, ObjectBuilder, ObjectCodec
  -- * Testing
  , reversible
  ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser, Pair, parseMaybe)
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Text as T
import Data.String

import Data.Codec (Codec(..), parse, produce)

type ObjectParser = ReaderT Object Parser
type ObjectBuilder = Const (Endo [ Pair ])
type ObjectCodec = Codec ObjectParser ObjectBuilder

type JSONCodec = Codec (ReaderT Value Parser) (Const Value)

pair :: ToJSON a => T.Text -> a -> ObjectBuilder ()
pair key val = Const $ Endo ((key .= val):)

-- | Turn an `ObjectCodec` into a `JSONCodec` with an expected name (see `withObject`).
obj :: String -> ObjectCodec a -> JSONCodec a
obj err (Codec r w) = Codec
  { parse = ReaderT $ withObject err $ runReaderT r
  , produce = \x -> Const $ object $ appEndo (getConst $ w x) []
  }

-- | Default JSON codec using @a@'s `ToJSON`/'FromJSON' instance.
def :: (ToJSON a, FromJSON a) => JSONCodec a
def = Codec (ReaderT parseJSON) (Const . toJSON)

-- | Read\/write a given value from/to a given key in the current object, using a given sub-codec.
entry :: T.Text -> JSONCodec a -> ObjectCodec a
entry key cd = Codec
  { parse = ReaderT $ \o -> (o .: key) >>= parseJSONCodec cd
  , produce = pair key . toJSONCodec cd
  }

instance (ToJSON a, FromJSON a) => IsString (ObjectCodec a) where
  fromString s = entry (fromString s) def

-- | Parse a JSON value with a `JSONCodec`, suitable for `FromJSON`.
parseJSONCodec :: JSONCodec a -> Value -> Parser a
parseJSONCodec = runReaderT . parse

-- | Produce a JSON value with a `JSONCodec`, suitable for `ToJSON`.
toJSONCodec :: JSONCodec a -> a -> Value
toJSONCodec cd = getConst . produce cd

-- | Verify that an `JSONCodec` is reversible for a given input.
reversible :: (Eq a, ToJSON a, FromJSON a) => JSONCodec a -> a -> Bool
reversible cd x
  = parseMaybe (parseJSONCodec cd) (toJSONCodec cd x) == Just x
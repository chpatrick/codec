module Data.Aeson.Codec
  (
  -- * JSON codecs
     JSONCodec
  -- * JSON object codecs
  , entry, pair, obj
  , ObjectParser, ObjectBuilder, ObjectCodec
  ) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser, Pair)
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Default.Class
import qualified Data.Text as T
import Data.String

import Data.Codec

-- | JSON codec. This is just a `ToJSON`/`FromJSON` implementation wrapped up in newtypes.
-- Use `def` to get a `JSONCodec` for a `ToJSON`/`FromJSON` instance.
type JSONCodec a = ConcreteCodec Value Parser a

instance (ToJSON a, FromJSON a) => Default (JSONCodec a) where
  def = Codec (ReaderT parseJSON) (Const . toJSON)

type ObjectParser = ReaderT Object Parser
type ObjectBuilder = Const (Endo [ Pair ])

-- | A codec that parses values out of a given `Object`, and produces
-- key-value pairs into a new one.
type ObjectCodec a = Codec ObjectParser ObjectBuilder a

-- | Produce a key-value pair.
pair :: ToJSON a => T.Text -> a -> ObjectBuilder ()
pair key val = Const $ Endo ((key .= val):)

-- | Read\/write a given value from/to a given key in the current object, using a given sub-codec.
-- ObjectCodec's `IsString` instance is equal to `entry` `def`.
entry :: T.Text -> JSONCodec a -> ObjectCodec a
entry key cd = Codec
  { parse = ReaderT $ \o -> (o .: key) >>= parseVal cd
  , produce = pair key . produceVal cd
  }

-- | Turn an `ObjectCodec` into a `JSONCodec` with an expected name (see `withObject`).
obj :: String -> ObjectCodec a -> JSONCodec a
obj err (Codec r w) = concrete
  (withObject err $ runReaderT r)
  (\x -> object $ appEndo (getConst $ w x) [])

instance (ToJSON a, FromJSON a) => IsString (ObjectCodec a) where
  fromString s = entry (fromString s) def

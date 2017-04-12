{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Aeson.Codec
  (
    JSONCodec(..)
  -- * JSON object codecs
  , ObjectParser, ObjectBuilder, ObjectCodec

  , entry
  , asObj
  , defJSON
  ) where

import           Control.Monad.Codec
import           Data.Aeson
import           Data.Aeson.Encoding
import           Data.Aeson.Types (Parser, Pair)
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Text as T

data JSONCodec a = JSONCodec
  { fromJSONCodec :: Value -> Parser a
  , toJSONCodec :: a -> Value
  , toEncodingCodec :: a -> Encoding
  }

-- | A `JSONCodec` based on `ToJSON` and `FromJSON`.
defJSON :: (FromJSON a, ToJSON a) => JSONCodec a
defJSON = JSONCodec
  { fromJSONCodec = parseJSON
  , toJSONCodec = toJSON
  , toEncodingCodec = toEncoding
  }

type ObjectParser = ReaderT Object Parser
type ObjectBuilder = Writer ( Series, [ Pair ] )
type ObjectCodec a = Codec ObjectParser ObjectBuilder a

-- | A codec that parses values out of a given `Object`, and produces
-- key-value pairs into a new one.
entry :: T.Text -> JSONCodec a -> ObjectCodec a
entry key valCodec = voidCodec
  (ReaderT $ \obj -> (obj .: key) >>= fromJSONCodec valCodec)
  (\val -> tell ( pair key (toEncodingCodec valCodec val), [ key .= toJSONCodec valCodec val ] ))

-- | Turn an `ObjectCodec` into a `JSONCodec` with an expected name (see `withObject`).
asObj :: String -> ObjectCodec a -> JSONCodec a
asObj err objCodec = JSONCodec
  { fromJSONCodec = withObject err (runReaderT (codecIn objCodec))
  , toJSONCodec = object . snd . execOut
  , toEncodingCodec = pairs . fst . execOut
  } where execOut = execWriter . codecOut objCodec

module Data.Aeson.Codec
  ( entry
  , parseObject, produceObject
  , ObjectParser, ObjectBuilder, ObjectCodec
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

-- | Parse an object with a given Codec or return the given error message when the value is not an object (see `withObject`).
-- Suitable for use as a `fromJSON` implementation.
parseObject :: Codec ObjectParser fw a -> String -> Value -> Parser a
parseObject cd objErr = withObject objErr (runReaderT (parse cd))

-- | Produce an object with a given Codec. Suitable for use as a `toJSON` implementation.
produceObject :: Codec fr ObjectBuilder a -> a -> Value
produceObject cd x = object $ appEndo (getConst $ produce cd x) []

-- | Read\/write a given value from/to a given key in the current object.
entry :: (ToJSON a, FromJSON a) => T.Text -> Codec ObjectParser ObjectBuilder a
entry key = Codec (ReaderT (.: key)) (\val -> Const $ Endo ((key .= val):))

instance (ToJSON a, FromJSON a) => IsString (Codec ObjectParser ObjectBuilder a) where
  fromString = entry . fromString

-- | Verify that an `ObjectCodec` is reversible for a given input.
reversible :: (Eq a, ToJSON a, FromJSON a) => ObjectCodec a -> a -> Bool
reversible cd x
  = parseMaybe (parseObject cd "Non-object produced.") (produceObject cd x) == Just x
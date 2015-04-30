module Data.Aeson.Codec where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser, Pair)
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Text as T
import Data.String

import Data.Codec (Codec, FieldCodec, parse, produce)

type ObjectParser = ReaderT Object Parser
type ObjectBuilder = Const (Endo [ Pair ])
type ObjectCodec a = Codec ObjectParser ObjectBuilder a

-- | Parse an object with a given Codec and error message when the value is not an object (see `withObject`).
-- Suitable for use as a `fromJSON` implementation.
parseObject :: Codec ObjectParser fw a -> String -> Value -> Parser a
parseObject cd objErr = withObject objErr (runReaderT (parse cd))

-- | Produce an object with a given Codec. Suitable for use as a `toJSON` implementation.
produceObject :: Codec fr ObjectBuilder a -> a -> Value
produceObject cd x = object $ appEndo (getConst $ produce cd x) []

-- | Field codec that maps a field to a JSON object entry with a given 'key'.
entry :: (ToJSON a, FromJSON a) => T.Text -> FieldCodec ObjectParser ObjectBuilder a
entry key = ( ReaderT (.: key), \val -> Const $ Endo ((key .= val):) )

instance (ToJSON a, FromJSON a) => IsString (FieldCodec ObjectParser ObjectBuilder a) where
  fromString = entry . fromString
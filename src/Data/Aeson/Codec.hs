{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Aeson.Codec
  ( JSONCodec(..)
  , defJSON

  -- * JSON object codecs
  , ObjectParser, ObjectBuilder, ObjectCodec
  , field, field'
  , asObject

  -- * JSON array codecs
  , ArrayParser, ArrayBuilder, ArrayCodec
  , element, element'
  , asArray
  , arrayOf, arrayOf'
  ) where

import           Control.Monad.Codec
import           Data.Aeson
import           Data.Aeson.Encoding
import qualified Data.Aeson.Encoding.Internal as AEI
import           Data.Aeson.Types (Parser, Pair)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer.Strict
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Describes the de/serialization of a type @a@. Equivalent to a `ToJSON` and a `FromJSON` instance.
data JSONCodec a = JSONCodec
  { parseJSONCodec :: Value -> Parser a
  , toJSONCodec :: a -> Value
  , toEncodingCodec :: a -> Encoding
  }

-- | Encode/decode a value with its `ToJSON` and `FromJSON` instances.
defJSON :: (FromJSON a, ToJSON a) => JSONCodec a
defJSON = JSONCodec
  { parseJSONCodec = parseJSON
  , toJSONCodec = toJSON
  , toEncodingCodec = toEncoding
  }

type ObjectParser = ReaderT Object Parser
type ObjectBuilder = Writer ( Series, Endo [ Pair ] )

-- | A codec that parses values out of a given `Object`, and produces
-- key-value pairs into a new one.
type ObjectCodec a = Codec ObjectParser ObjectBuilder a

-- | Store/retrieve a value in a given JSON field, with a given JSONCodec.
field' :: T.Text -> JSONCodec a -> ObjectCodec a
field' key valCodec = Codec
  { codecIn = ReaderT $ \obj -> (obj .: key) >>= parseJSONCodec valCodec
  , codecOut = \val ->
    writer
      ( val
      , ( pair key (toEncodingCodec valCodec val)
        , Endo ((key .= toJSONCodec valCodec val) :)
        )
      )
  }

-- | Store/retrieve a value in a given JSON field, with the default JSON serialization.
field :: (FromJSON a, ToJSON a) => T.Text -> ObjectCodec a
field key = field' key defJSON

-- | Turn an `ObjectCodec` into a `JSONCodec` with an expected name (see `withObject`).
asObject :: String -> ObjectCodec a -> JSONCodec a
asObject err objCodec = JSONCodec
  { parseJSONCodec = withObject err (runReaderT (codecIn objCodec))
  , toJSONCodec = object . (`appEndo` []) . snd . execOut
  , toEncodingCodec = pairs . fst . execOut
  } where execOut = execWriter . codecOut objCodec

type ArrayParser = StateT [ Value ] Parser
type ArrayBuilder = Writer ( Series, [ Value ] )
type ArrayCodec a = Codec ArrayParser ArrayBuilder a

-- | Expect/append an array element, using a given `JSONCodec`.
element' :: JSONCodec a -> ArrayCodec a
element' valCodec = Codec
  { codecIn = StateT $ \case
    [] -> fail "Expected an element, got an empty list."
    x : xs -> do
      val <- parseJSONCodec valCodec x
      return ( val, xs )

  , codecOut = \val -> writer ( val, ( AEI.Value $ AEI.retagEncoding $ toEncodingCodec valCodec val, [ toJSONCodec valCodec val ] ) )
  }

element :: (FromJSON a, ToJSON a) => ArrayCodec a
element = element' defJSON

-- | A codec that parses values out of a given `Array`, and produces
-- key-value pairs into a new one.
asArray :: String -> ArrayCodec a -> JSONCodec a
asArray err arrCodec = JSONCodec
  { parseJSONCodec = withArray err $ \arr -> do
      ( val, leftover ) <- runStateT (codecIn arrCodec) (V.toList arr)
      unless (null leftover) $ fail "Elements left over in parsed array."
      return val
  , toJSONCodec = Array . V.fromList . snd . execOut
  , toEncodingCodec = \val -> case fst (execOut val) of
      AEI.Empty -> emptyArray_
      AEI.Value enc -> AEI.wrapArray enc
  } where execOut = execWriter . codecOut arrCodec

-- | Given a `JSONCodec` for @b@ and a way to turn @a@ into @[ b ]@ and back,
-- create a `JSONCodec` for @a@.
arrayOf' :: (a -> [ b ]) -> ([ b ] -> a) -> JSONCodec b -> JSONCodec a
arrayOf' aToList listToA elemCodec = JSONCodec
  { parseJSONCodec = \arr -> do
      vals <- parseJSON arr
      parsedVals <- traverse (parseJSONCodec elemCodec) (vals :: [ Value ])
      return (listToA parsedVals)
  , toJSONCodec = Array . V.fromList . map (toJSONCodec elemCodec) . aToList
  , toEncodingCodec = AEI.list (toEncodingCodec elemCodec) . aToList
  }

-- | Given a a way to turn @a@ into @[ b ]@ and back, create a `JSONCodec` for @a@.
arrayOf :: (FromJSON b, ToJSON b) => (a -> [ b ]) -> ([ b ] -> a) -> JSONCodec a
arrayOf aToList listToA = arrayOf' aToList listToA defJSON
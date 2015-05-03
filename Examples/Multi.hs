{-# LANGUAGE OverloadedStrings #-}

module Examples.Multi where

import Data.Aeson
import Data.Aeson.Codec
import Data.Codec

data Multi a
  = Con1 { foo :: String, bar :: Int }
  | Con2 { baz :: a }
  | Con3
  deriving Show

genFields ''Multi

multiCodec :: (ToJSON a, FromJSON a) => JSONCodec (Multi a)
multiCodec = obj "multi object" $ covered $ (
  cbuild c_Con1
    $   f_foo >-< "foo"
    >>> f_bar >-< "bar"
  ) <-> (
  cbuild c_Con2
    $ f_baz >-< "baz"
  ) <-> (
  cbuild c_Con3 done
  )

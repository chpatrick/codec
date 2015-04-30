{-# LANGUAGE OverloadedStrings #-}

module Data.Codec.JSON where

import Data.Aeson
import Data.Aeson.Codec
import Data.Codec
import Data.Codec.TH
import Data.Text (Text)

data User = User
  { username :: Text
  , userEmail :: Text
  , userLanguages :: [ Text ]
  } deriving Show

genFields ''User

userCodec :: ObjectCodec User
userCodec = codec User
  $   f_username      >< "user"
  >>> f_userEmail     >< "email"
  >>> f_userLanguages >< "languages"

instance FromJSON User where
  parseJSON = parseObject userCodec "Expected a user object"

instance ToJSON User where
  toJSON = produceObject userCodec
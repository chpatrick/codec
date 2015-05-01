{-# LANGUAGE OverloadedStrings #-}

module Data.Codec.JSON where

import Data.Aeson
import Data.Aeson.Codec
import Data.Codec
import Data.Text (Text)

data User = User
  { username :: Text
  , userEmail :: Text
  , userLanguages :: [ Text ]
  , userReferrer :: Maybe User
  } deriving Show

genFields ''User

userCodec :: JSONCodec User
userCodec = obj "user object" $ codec User
  $   f_username      >< "user" -- entry with FromJSON/ToJSON serialization
  >>> f_userEmail     >< "email"
  >>> f_userLanguages >< "languages"
  >>> f_userReferrer  >< opt (entry "referrer" userCodec) -- entry with specific codec

instance FromJSON User where
  parseJSON = parseJSONCodec userCodec

instance ToJSON User where
  toJSON = toJSONCodec userCodec
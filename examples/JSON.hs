{-# LANGUAGE OverloadedStrings #-}

module Data.Codec.Examples.JSON where

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
userCodec = obj "user object" $ finish User
  $   f_username      >-< "user" -- entry with FromJSON/ToJSON serialization
  >>> f_userEmail     >-< "email"
  >>> f_userLanguages >-< "languages"
  >>> f_userReferrer  >-< opt (entry "referrer" userCodec) -- entry with specific codec

instance FromJSON User where
  parseJSON = produceVal userCodec

instance ToJSON User where
  toJSON = parseVal userCodec
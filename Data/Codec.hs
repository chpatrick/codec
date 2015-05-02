module Data.Codec (
  -- $constructing
    module Data.Codec.Field
  , module Data.Codec.Codec
  , module Data.Codec.TH
  , module Data.Codec.Tuple
  ) where

import Data.Codec.Field 
import Data.Codec.Codec
import Data.Codec.TH
import Data.Codec.Tuple

-- $constructing
-- The main purpose of this package is to make the creation of `Codec`s as easy and painless as possible.
-- If we have a data type such as:
--
-- @
-- data User = User
--  { username :: Text
--  , userEmail :: Text
--  , userLanguages :: [ Text ]
--  , userReferrer :: Maybe Text
--  } deriving Show
-- @
--
-- we can use the `genFields` function to generate `Field`s for each record field:
--
-- @
-- genFields ''User
-- @
--
-- This will create `Field`s named @f_username@, @f_userEmail@, etc. These fields can be associated with an
-- appropriate `Codec` with the `>-<` operator to specify the representation of the data structure. These
-- associations can then be combined with the `>>>` operator in the order of serialization/deserialization.
-- These associations can then be finalized into a `Codec` by providng the constructor to use.
-- For example, using the JSON `entry` `Codec` that assigns a value to a JSON key, we could write a codec for
-- @User@ as:
--
-- @
--  userCodec :: JSONCodec User
--  userCodec = obj "user object' $ finish User
--    $   f_username      >-< "user"
--    >>> f_userEmail     >-< "email"
--    >>> f_userLanguages >-< "languages"
--    >>> f_userReferrer  >-< opt "referrer"
-- @
--
-- The type system ensures that every field is provided exactly once.
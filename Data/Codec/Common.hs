{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, LambdaCase, TemplateHaskell #-}

module Data.Codec.Common
  ( 
  -- $constructing
  -- * Codecs
    Codec(..)
  , always, mapCodec, mapCodecM, opt
  -- * Constructing Codecs
  , (><), (>>>)
  , Buildable(..)
  , Field(..)
  , Build(..), build, build_, codec
  , X(X)
  )
where

import Control.Applicative (Alternative(..), optional)
import Control.Category (Category(..), (>>>))
import Prelude hiding (id, (.))

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
-- appropriate `Codec` with the `><` operator to specify the representation of the data structure. These
-- associations can then be combined with the `>>>` operator in the order of serialization/deserialization.
-- These associations can then be finalized into a `Codec` by providng the constructor to use.
-- For example, using the JSON `entry` `Codec` that assigns a value to a JSON key, we could write a codec for
-- @User@ as:
--
-- @
--  userCodec :: Codec ObjectParser ObjectReader User
--  userCodec = codec User
--    $   f_username      >< entry "user"
--    >>> f_userEmail     >< entry "email"
--    >>> f_userLanguages >< entry "languages"
--    >>> f_userReferrer  >< opt (entry "referrer")
-- @
--
-- The type system ensures that every field is provided exactly once.

-- | `Field`s partially apply constructors and replace an argument with this type.
data X = X

-- | The class of constructor applications that have been completely filled in by composing
-- `Build`s. If you see an error message involving this, it means that you forgot to specify
-- a `Build` for a field.
class Buildable r a where
  give :: a -> r

instance Buildable r b => Buildable r (X -> b) where
  give f = give $ f X

instance Buildable r r where
  give = id

-- | Describes how to apply a constructor argument and how to extract from a record.
-- @y@ should be @x@ with one argument knocked out: e. g.
--
-- @
-- Field MyType Int (Int -> a2 -> MyType) (X -> a2 -> MyType)
-- @
data Field r a x y = Field (a -> x -> y) (r -> a)

-- | Field application equipped with serializers and deserializers.
data Build fr fw r x y = Build (fr (x -> y)) (r -> fw ())

-- | De/serializer for a given type.
data Codec fr fw r = Codec { parse :: fr r, produce :: r -> fw () }

-- | Turn a `Field` into a `Build` with the given serializers.
build :: Functor fr => Field r a x y -> fr a -> (a -> fw ()) -> Build fr fw r x y
build (Field ct ex) r w = Build (ct <$> r) (w . ex)

-- | Promote a pair of de/serialization actions to a `Build`.
build_ :: Functor fr => fr () -> fw () -> Build fr fw r x x
build_ r w = Build (id <$ r) (const w)

-- Category instance for Build to make it composable
-- actions are sequenced in the `>>>` direction, so reversed here
instance (Applicative fr, Applicative fw) => Category (Build fr fw r) where
  id = Build (pure id) (const $ pure ())
  Build r1 w1 . Build r2 w2
    = Build ((>>>) <$> r2 <*> r1) (\x -> w2 x *> w1 x)

-- | Turn a `Build` into a `Codec` with a given constructor.
codec :: (Functor fr, Buildable r y) => x -> Build fr fw r x y -> Codec fr fw r
codec f (Build r w) = Codec ((\g -> give $ g f) <$> r) w

-- | Apply a `Codec` to a `Field` to produce a `Build`.
(><) :: Functor fr => Field r a x y -> Codec fr fw a -> Build fr fw r x y
a >< Codec r w = build a r w

-- | A `Codec` that always deserializes the given value and serializes nothing.
always :: (Applicative fr, Applicative fw) => a -> Codec fr fw a
always x = Codec (pure x) (const $ pure ())

-- | Map a `Codec` by providing an isomorphism between @a@ and @b@.
mapCodec :: Functor fr => (a -> b) -> (b -> a) -> Codec fr fw a -> Codec fr fw b
mapCodec to from (Codec r w)
  = Codec (to <$> r) (w . from)

-- | Map a field codec monadically. Useful for error handling but care must be taken to make sure that
-- the results are still complementary.
mapCodecM :: (Monad fr, Monad fw) => (a -> fr b) -> (b -> fw a) -> Codec fr fw a -> Codec fr fw b
mapCodecM to from (Codec r w)
  = Codec (r >>= to) (\x -> from x >>= w)

-- | Given a `Codec` for @a@, make one for `Maybe` @a@ that applies its deserializer optionally
-- and does nothing when serializing `Nothing`.
opt :: (Alternative fr, Applicative fw) => Codec fr fw a -> Codec fr fw (Maybe a)
opt (Codec r w) = Codec (optional r) (maybe (pure ()) w)
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, LambdaCase, TemplateHaskell #-}

module Data.Codec 
  ( X(X), Buildable(..)
  , Field(..)
  , Build(..), build, build_
  , Codec(..), codec
  , FieldCodec, (><), always, mapCodec, mapCodecM
  , (>>>)
  )
where

import Control.Category (Category(..), (>>>))
import Prelude hiding (id, (.))

-- | `Field`s partially apply constructors and replace the arguments with this type.
data X = X

-- | If a function only takes `X`s as input, we can get the output.
class Buildable r a where
  give :: a -> r

instance Buildable r b => Buildable r (X -> b) where
  give f = give $ f X

instance Buildable r r where
  give = id

-- | Describes how to apply a constructor argument and how to extract from a record.
-- 'y' should be 'x' with one argument knocked out: e. g.
-- @x: (Int -> a2 -> MyRecord) y: (X -> a2 -> MyRecord)@
data Field r a x y = Field (a -> x -> y) (r -> a)

-- | Field application equipped with serializers and deserializers.
data Build fr fw r x y = Build (fr (x -> y)) (r -> fw ())

-- | De/serializer for a given type.
data Codec fr fw r = Codec { parse :: fr r, produce :: r -> fw () }

-- | Turn a `Field` into a `Build` with the given serializers.
build :: Functor fr => Field r a x y -> fr a -> (a -> fw ()) -> Build fr fw r x y
build (Field ct ex) r w = Build (ct <$> r) (w . ex)

-- | Promote a pair of de/serialization actions into a `Build`.
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

-- Niceties

-- | A pair of complementary de/serializers.
type FieldCodec fr fw a = ( fr a, a -> fw () )

-- | Apply a `FieldCodec` to a `Field` to produce a `Build`.
(><) :: Functor fr => Field r a x y -> FieldCodec fr fw a -> Build fr fw r x y
(><) a = uncurry (build a)

-- | A `FieldCodec` that always deserializes the given value and serializes nothing.
always :: (Applicative fr, Applicative fw) => a -> FieldCodec fr fw a
always x = ( pure x, const $ pure () )

-- | Map a `FieldCodec` by providing an isomorphism between 'a' and 'b'.
mapCodec :: Functor fr => (a -> b) -> (b -> a) -> FieldCodec fr fw a -> FieldCodec fr fw b
mapCodec to from ( r, w )
  = ( to <$> r, w . from )

-- | Map a field codec monadically. Useful for error handling but care must be taken to make sure that
-- the results are still complementary.
mapCodecM :: (Monad fr, Monad fw) => (a -> fr b) -> (b -> fw a) -> FieldCodec fr fw a -> FieldCodec fr fw b
mapCodecM to from ( r, w )
  = ( r >>= to, \x -> from x >>= w )
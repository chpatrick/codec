{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, LambdaCase, TemplateHaskell #-}

module Data.Codec 
  (X(X)
  , KO(..)
  , Field(..)
  , Build(..), build, build_
  , Codec(..), codec
  , FieldCodec, (><), mapCodec, mapCodecM
  , (>>>)
  )
where

import Control.Category (Category(..), (>>>))
import Prelude hiding (id, (.))

-- a "knocked-out" constructor parameter (we could use ())
data X = X

-- produce an r from a fully knocked-out function
class KO r a where
  give :: a -> r

instance KO r b => KO r (X -> b) where
  give f = give $ f X

instance KO r r where
  give = id

-- describes how to apply a constructor argument and how to extract from a record
-- y should be x with one argument knocked out: e. g.
-- x: (Int -> a2 -> MyRecord) y: (X -> a2 -> MyRecord)
data Field r a x y = Field (a -> x -> y) (r -> a)

-- Field application equipped with serializers and deserializers
data Build fr fw r x y = Build (fr (x -> y)) (r -> fw ())

-- Finished product with serializer and deserializer
data Codec fr fw r = Codec { parse :: fr r, produce :: r -> fw () }

-- turn an Field into an Build with the given serializers
build :: Functor fr => Field r a x y -> fr a -> (a -> fw ()) -> Build fr fw r x y
build (Field ct ex) r w = Build (ct <$> r) (w . ex)

-- promote a pair of de/serialization actions into a Build
build_ :: Functor fr => fr () -> fw () -> Build fr fw r x x
build_ r w = Build (id <$ r) (const w)

-- Category instance for Build to make it composable
-- actions are sequenced in the ">>>" direction, so reversed here
instance (Applicative fr, Applicative fw) => Category (Build fr fw r) where
  id = Build (pure id) (const $ pure ())
  Build r1 w1 . Build r2 w2
    = Build ((>>>) <$> r2 <*> r1) (\x -> w2 x *> w1 x)

-- turn a Build into a Codec with a given constructor
codec :: (Functor fr, KO r y) => x -> Build fr fw r x y -> Codec fr fw r
codec f (Build r w) = Codec ((\g -> give $ g f) <$> r) w

-- Niceties

-- A pair of complementary serializers/deserializers
type FieldCodec fr fw a = ( fr a, a -> fw () )

-- Apply a pair to an Field to produce an Build
(><) :: Functor fr => Field r a x y -> FieldCodec fr fw a -> Build fr fw r x y
(><) a = uncurry (build a)

mapCodec :: Functor fr => (a -> b) -> (b -> a) -> FieldCodec fr fw a -> FieldCodec fr fw b
mapCodec to from ( r, w )
  = ( to <$> r, w . from )

-- Map a field codec monadically. Useful for error handling but care must be taken to make sure that
-- the results are still complementary.
mapCodecM :: (Monad fr, Monad fw) => (a -> fr b) -> (b -> fw a) -> FieldCodec fr fw a -> FieldCodec fr fw b
mapCodecM to from ( r, w )
  = ( r >>= to, \x -> from x >>= w )
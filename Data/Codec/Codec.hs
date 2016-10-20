module Data.Codec.Codec
  ( -- * Codecs
    Codec'(..), Codec
  , (>-<)
    -- * Concrete codecs
  , ConcreteCodec, concrete, parseVal, produceVal
    -- * Partial codecs
    -- | Partial codecs are useful for creating codecs
    -- for types with multiple constructors. See @examples/Multi.hs@.
  , PartialCodec, cbuild, assume, covered, (<->), produceMaybe
    -- * Codec combinators
  , opt, mapCodec, mapCodecF, mapCodecM
  , mapCodec', comapCodec', (=.)
  )
where

import Control.Applicative
import Control.Monad ((>=>))
import Control.Monad.Reader (ReaderT(..))
import Data.Codec.Field
import Data.Functor.Compose
import Data.Maybe (fromMaybe)

-- | De/serializer for the given types. Usually w ~ r, but they are separate
-- to allow for an `Applicative` instance.
data Codec' fr fw w r = Codec
  { parse :: fr r
  , produce :: w -> fw () 
  }
  deriving Functor

-- | De/serializer for @a@.
type Codec fr fw a = Codec' fr fw a a

-- Build up a serializer in parallel to a deserializer.
instance (Applicative fw, Applicative fr) => Applicative (Codec' fr fw w) where
  pure x = Codec (pure x) (const $ pure ())
  Codec f fw <*> Codec x xw
    = Codec (f <*> x) (\w -> fw w *> xw w)

-- | Associate a `Field` with a `Codec` to create a `Codec` `Build`.
(>-<) :: Functor fr => Field r a x y -> Codec fr fw a -> Build r (Codec' fr fw r) x y
Field c g >-< Codec r w
  = Build (c <$> Codec r (w . g))

-- Codec combinators

-- | Given a `Codec` for @a@, make one for `Maybe` @a@ that applies its deserializer optionally
-- and does nothing when serializing `Nothing`.
opt :: (Alternative fr, Applicative fw) => Codec fr fw a -> Codec fr fw (Maybe a)
opt (Codec r w) = Codec (optional r) (maybe (pure ()) w)

-- | Turn a @`Codec` a@ into a @`Codec` b@ by providing an isomorphism.
mapCodec :: Functor fr => (a -> b) -> (b -> a) -> Codec fr fw a -> Codec fr fw b
mapCodec = mapCodec'

-- | Map a field codec monadically. Useful for error handling but care must be taken to make sure that
-- the results are still complementary.
mapCodecM :: (Monad fr, Monad fw) => (a -> fr b) -> (b -> fw a) -> Codec fr fw a -> Codec fr fw b
mapCodecM to from (Codec r w)
  = Codec (r >>= to) (from >=> w)

-- | Map the contexts of a given `Codec`.
mapCodecF :: (fr a -> gr a) -> (fw () -> gw ()) -> Codec fr fw a -> Codec gr gw a
mapCodecF fr fw (Codec r w)
  = Codec (fr r) (fw . w)

-- | Independently map the two components of a `Codec'`.
--
-- Generalizes `mapCodec`.
mapCodec' :: Functor fr => (a -> b) -> (c -> d) -> Codec' fr fw d a -> Codec' fr fw c b
mapCodec' to from (Codec r w)
  = Codec (to <$> r) (w . from)

-- | Map on the `produce` component of a `Codec`.
--
-- @
-- comapCodec' = mapCodec' id
-- @
--
-- But `comapCodec'` does not require a `Functor` constraint.
comapCodec' :: (c -> d) -> Codec' fr fw d a -> Codec' fr fw c a
comapCodec' from (Codec r w)
  = Codec r (w . from)

-- | Infix synonym of `comapCodec'`.
--
-- The symbol mimics a record-like syntax in applicative definitions.
(=.) :: (b -> a') -> Codec' fr fw a' a -> Codec' fr fw b a
(=.) = comapCodec'

infixr 5 =.

-- | A codec where `a` can be produced from a concrete value of `b` in context `f`,
-- and a concrete type of value `b` can always be produced.
type ConcreteCodec b f a = Codec (ReaderT b f) (Const b) a

-- | Create a concrete codec from a reader and a writer.
concrete :: (b -> f a) -> (a -> b) -> ConcreteCodec b f a
concrete r w = Codec (ReaderT r) (Const . w)

-- | Parse a concrete value with a given `ConcreteCodec`.
parseVal :: ConcreteCodec b f a -> b -> f a
parseVal (Codec r _) = runReaderT r

-- | Produce a concrete value with a given `ConcreteCodec`.
produceVal :: ConcreteCodec b f a -> a -> b
produceVal (Codec _ w) = getConst . w

-- | A codec that can only serialize a subset of values.
type PartialCodec fr fw a = Codec fr (Compose Maybe fw) a

-- | Finish a codec construction with a @`Con` r@ to produce a `PartialCodec`.
-- This will check that the given record has the appropriate constructor
-- before serializing.
cbuild :: (Functor fr, Buildable r y)
  => Con r x -> Build r (Codec' fr fw r) x y -> PartialCodec fr fw r
cbuild (Con c p) = assume p . build c

-- | Guard a `Codec` with a predicate to create a `PartialCodec`.
assume :: (a -> Bool) -> Codec fr fw a -> PartialCodec fr fw a
assume p (Codec r w)
  = Codec r (\x -> Compose $ if p x then Just (w x) else Nothing)

-- | Convert a `PartialCodec` into a `Codec`, throwing an error
-- on values it cannot serialize.
covered :: PartialCodec fr fw a -> Codec fr fw a
covered cd
  = Codec (parse cd) (fromMaybe (error "Could not serialize value.") . produceMaybe cd)

-- | Combine alternative `PartialCodec`s.
(<->) :: Alternative fr => PartialCodec fr fw a -> PartialCodec fr fw a -> PartialCodec fr fw a
cd <-> acd = Codec
  { parse = parse cd <|> parse acd
  , produce = \x -> Compose $ produceMaybe cd x <|> produceMaybe acd x
}

-- | Attempt to get a serialization for a given value.
produceMaybe :: PartialCodec fr fw a -> a -> Maybe (fw ())
produceMaybe (Codec _ w) x
  = getCompose (w x)

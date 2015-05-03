module Data.Codec.Codec
  ( -- * Codecs
    Codec'(..), Codec
  , (>-<)
    -- * Concrete codecs
  , ConcreteCodec, concrete, parseVal, produceVal
    -- * Codec combinators
  , opt, mapCodec, mapCodecM
  )
where

import Control.Applicative (Alternative(..), optional, Const(..))
import Control.Monad ((>=>))
import Control.Monad.Reader (ReaderT(..))
import Data.Codec.Field (Field(..), Build(..))

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
mapCodec to from (Codec r w)
  = Codec (to <$> r) (w . from)

-- | Map a field codec monadically. Useful for error handling but care must be taken to make sure that
-- the results are still complementary.
mapCodecM :: (Monad fr, Monad fw) => (a -> fr b) -> (b -> fw a) -> Codec fr fw a -> Codec fr fw b
mapCodecM to from (Codec r w)
  = Codec (r >>= to) (from >=> w)

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

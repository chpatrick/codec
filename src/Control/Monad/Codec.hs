{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Codec
  ( CodecFor(..)
  , Codec
  , (=.)
  , fmapArg
  ) where

import           Data.Profunctor

-- | A serializer/deserializer pair reading @a@ in context @r@ and writing @c@ in context @w@.
data CodecFor r w c a = Codec
  { codecIn :: r a
  , codecOut :: c -> w a
  } deriving (Functor)

type Codec r w a = CodecFor r w a a

instance (Applicative r, Applicative w) => Applicative (CodecFor r w c) where
  pure x = Codec
    { codecIn = pure x
    , codecOut = \_ -> pure x
    }
  f <*> x = Codec
    { codecIn = codecIn f <*> codecIn x
    , codecOut = \c -> codecOut f c <*> codecOut x c
    }

instance (Monad r, Monad w) => Monad (CodecFor r w c) where
  return = pure
  m >>= f = Codec
    { codecIn = codecIn m >>= \x -> codecIn (f x)
    , codecOut = \c -> codecOut m c >>= \x -> codecOut (f x) c
    }

instance (Functor r, Functor w) => Profunctor (CodecFor r w) where
  dimap fIn fOut Codec {..} = Codec
    { codecIn = fmap fOut codecIn
    , codecOut = fmap fOut . codecOut . fIn
    }

-- | Compose a function into the serializer of a `Codec`.
-- Useful to modify a `Codec` so that it writes a particular record field.
(=.) :: (c' -> c) -> CodecFor r w c a -> CodecFor r w c' a
fIn =. codec = codec { codecOut = codecOut codec . fIn }

-- | Modify a serializer function so that it also returns the serialized value,
-- Useful for implementing codecs.
fmapArg :: Functor f => (a -> f ()) -> a -> f a
fmapArg f x = x <$ f x

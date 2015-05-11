{-# LANGUAGE RankNTypes #-}

module Control.Lens.Codec (lensCodec) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import Data.Codec

-- | Turn a `Lens` into a `Codec` operating in a `MonadReader`/`MonadState`.
lensCodec :: (MonadReader s fr, MonadState s fw)
  => (forall f. (a -> f a) -> s -> f s) -> Codec fr fw a
lensCodec l = Codec
  { parse = liftM getConst $ asks (l Const)
  , produce = \x -> modify (runIdentity . l (const $ Identity x))
  }

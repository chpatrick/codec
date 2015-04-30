{-# LANGUAGE DataKinds, GADTs #-}

module Data.Codec.Iso (iso, IsoBuild(..), (>-<)) where

import Control.Category
import Prelude hiding (id, (.))

import Data.Codec

-- | Generate an isomorphism between two types by providing constructors for each and an `IsoBuild` representing matching fields.
iso :: (Buildable a ay, Buildable b by)
  => ax -> bx
  -> IsoBuild a b '( ax, bx ) '( ay, by )
  -> ( a -> b, b -> a )
iso ac bc (IsoBuild aca bca)
  = ( \a -> give $ bca a bc, \b -> give $ aca b ac )
iso ac bc IsoId = ( const (give bc), const (give ac) )

instance Category (IsoBuild a b) where
  id = IsoId
  IsoBuild ac bc . IsoBuild ac' bc'
    = IsoBuild ((.) <$> ac <*> ac') ((.) <$> bc <*> bc')
  IsoId . ib' = ib'
  ib . IsoId = ib

-- | Build an isomorphism between `a` and `b`.
data IsoBuild a b xx yy where
  IsoId :: IsoBuild a b xx xx -- workaround for awkwardness with DataKinds
  IsoBuild :: (b -> (ax -> ay)) -> (a -> (bx -> by)) -> IsoBuild a b '( ax, bx ) '( ay, by )

-- | Associate fields in `a` and `b` to produce an `IsoBuild`.
(>-<) :: Field a f ax ay -> Field b f bx by -> IsoBuild a b '( ax, bx ) '( ay, by )
Field ac ae >-< Field bc be
  = IsoBuild (ac . be) (bc . ae)
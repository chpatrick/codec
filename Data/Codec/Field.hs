module Data.Codec.Field
  ( 
  -- * First-class record construction
    Field(..)
  , Build(..)
  , Con(..)
  , ($>>), (>>>), done
  , X(X), Buildable(..)
  , having, build
  ) where

import Control.Category
import Prelude hiding ((.), id)

-- | `Field`s partially apply constructors and replace arguments with this type.
data X = X

-- | The class of constructor applications that have been completely filled in by composing
-- `Build`s. If you see an error message involving this, it means that you forgot to specify
-- a `Build` for a field.
class Buildable r a where
  give :: a -> r

instance Buildable r r where
  give = id

instance Buildable r b => Buildable r (X -> b) where
  give f = give $ f X

-- | Describes how to apply a constructor argument and how to extract from a record.
-- @y@ should be @x@ with one argument knocked out: e. g.
--
-- @
-- Field MyType Int (Int -> a2 -> MyType) (X -> a2 -> MyType)
-- @
data Field r a x y = Field (a -> x -> y) (r -> a)

-- Static (Backwards f) + phantom parameter
-- | An ongoing record construction of an @r@ in context @f@.
-- Applicative actions are sequenced in the direction of `>>>`.
newtype Build r f x y = Build (f (x -> y))

-- | Combine a `Field` and a way to produce an @a@ to get a `Build`.
having :: Functor f => Field r a x y -> f a -> Build r f x y
having (Field c _) p = Build (c <$> p)

-- | No-op `Build` (same as `id`).
done :: Applicative f => Build r f x x
done = id

instance Applicative f => Category (Build r f) where
  id = Build (pure id)
  Build f . Build g
    = Build ((>>>) <$> g <*> f)

-- | Finish a construction given a constructor.
build :: (Functor f, Buildable r y) => x -> Build r f x y -> f r
build x (Build b)
  = (\f -> give $ f x) <$> b

-- | Infix version of `build`.
($>>) :: (Functor f, Buildable r y) => x -> Build r f x y -> f r
($>>) = build
infixr 1 $>>

-- | A constructor for a given record and a way to check whether it has it.
data Con r x = Con x (r -> Bool)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Monad.Codec.Generic
    ( SimplifyGeneric(..)
    , match
    , rchoose
    , lchoose
    , combine
    , (|*|)
    , (?>)
    , (<?)
    ) where

import           Control.Applicative
import           Control.Monad.Codec
import           Data.Profunctor
import           GHC.Generics

-- | A class providing functions to convert between generics and structures made of `Either` and ( , )
class SimplifyGeneric a where
    type SimpleGeneric a :: *
    simplifyGeneric :: a p -> SimpleGeneric a
    unsimplifyGeneric :: SimpleGeneric a -> a p

instance SimplifyGeneric U1 where
    type SimpleGeneric U1 = ()
    simplifyGeneric = const ()
    unsimplifyGeneric = const U1

instance SimplifyGeneric (K1 i c) where
    type SimpleGeneric (K1 i c) = c
    simplifyGeneric = unK1
    unsimplifyGeneric = K1

instance SimplifyGeneric f => SimplifyGeneric (M1 i c f) where
    type SimpleGeneric (M1 i c f) = SimpleGeneric f
    simplifyGeneric = simplifyGeneric . unM1
    unsimplifyGeneric = M1 . unsimplifyGeneric

instance (SimplifyGeneric a, SimplifyGeneric b) =>
         SimplifyGeneric (a :+: b) where
    type SimpleGeneric (a :+: b) = Either (SimpleGeneric a) (SimpleGeneric b)
    simplifyGeneric (L1 x) = Left $ simplifyGeneric x
    simplifyGeneric (R1 x) = Right $ simplifyGeneric x
    unsimplifyGeneric (Left x)  = L1 $ unsimplifyGeneric x
    unsimplifyGeneric (Right x) = R1 $ unsimplifyGeneric x

instance (SimplifyGeneric a, SimplifyGeneric b) =>
         SimplifyGeneric (a :*: b) where
    type SimpleGeneric (a :*: b) = (SimpleGeneric a, SimpleGeneric b)
    simplifyGeneric (a :*: b) = (simplifyGeneric a, simplifyGeneric b)
    unsimplifyGeneric (a, b) = unsimplifyGeneric a :*: unsimplifyGeneric b

-- | Construct a codec for any type using the `Generic` instance.
-- The codec is usually obtained by using `combine`, `lchoose` and `rchoose`
match ::
       (Generic a, Functor r, Functor w, SimplifyGeneric (Rep a))
    => Codec r w (SimpleGeneric (Rep a))
    -> Codec r w a
match = dimap (simplifyGeneric . from) (to . unsimplifyGeneric)

-- | Combine two codecs to read / write a tuple of values
combine ::
       (Applicative r, Applicative w)
    => CodecFor r w a b
    -> CodecFor r w c d
    -> CodecFor r w (a, c) (b, d)
combine a b = (,) <$> fst =. a <*> snd =. b

-- | Operator for `combine`
(|*|) ::
       (Applicative r, Applicative w)
    => CodecFor r w a b
    -> CodecFor r w c d
    -> CodecFor r w (a, c) (b, d)
(|*|) = combine

infixl 7 |*|

-- | Combine two codecs to read / write `Either` using `Left` as default when reading
lchoose ::
       (Alternative r, Functor w)
    => Codec r w a
    -> Codec r w b
    -> Codec r w (Either a b)
lchoose l r =
    Codec
    { codecIn = (Left <$> codecIn l) <|> (Right <$> codecIn r)
    , codecOut = either (fmap Left . codecOut l) (fmap Right . codecOut r)
    }

-- | Operator for `lchoose`
(?>) ::
       (Alternative r, Functor w)
    => Codec r w a
    -> Codec r w b
    -> Codec r w (Either a b)
(?>) = lchoose

infixl 6 ?>

-- | Combine two codecs to read / write `Either` using `Right` as default when reading
rchoose ::
       (Alternative r, Functor w)
    => Codec r w a
    -> Codec r w b
    -> Codec r w (Either a b)
rchoose l r =
    Codec
    { codecIn = (Right <$> codecIn r) <|> (Left <$> codecIn l)
    , codecOut = either (fmap Left . codecOut l) (fmap Right . codecOut r)
    }

-- | Operator for `rchoose`
(<?) ::
       (Alternative r, Functor w)
    => Codec r w a
    -> Codec r w b
    -> Codec r w (Either a b)
(<?) = rchoose

infixl 6 <?

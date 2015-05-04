module Data.Codec.Tuple
  ( Field1(..), Field2(..)
  , c_Left, c_Right, f_left, f_right
  ) where

import Data.Either

import Data.Codec.Field (Field(..), Con(..), X)

class Field1 r a x y | r x -> y, r y -> x, r -> a, x y -> r where
  f_1 :: Field r a x y

instance Field1 ( a, b ) a (a -> a2 -> ( a, b )) (X -> a2 -> ( a, b )) where
  f_1 = Field (\x c _ a2 -> c x a2) fst

class Field2 r a x y | r x -> y, r y -> x, r -> a, x y -> r where
  f_2 :: Field r a x y

instance Field2 ( a, b ) b (a1 -> b -> ( a, b )) (a1 -> X -> ( a, b )) where
  f_2 = Field (\x c a1 _ -> c a1 x) snd

c_Left :: Con (Either a b) (a -> Either a b)
c_Left = Con Left isLeft

c_Right :: Con (Either a b) (b -> Either a b)
c_Right = Con Right isRight

f_left :: Field (Either a b) a (a -> Either a b) (X -> Either a b)
f_left = Field (\x c _ -> c x) (\(Left l) -> l)

f_right :: Field (Either a b) b (b -> Either a b) (X -> Either a b)
f_right = Field (\x c _ -> c x) (\(Right l) -> l)
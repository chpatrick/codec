module Data.Codec.Tuple(Field1(..), Field2(..)) where

import Data.Codec.Common

class Field1 r a x y | r x -> y, r y -> x, r -> a, x y -> r where
  f_1 :: Field r a x y

instance Field1 ( a, b ) a (a -> a2 -> ( a, b )) (X -> a2 -> ( a, b )) where
  f_1 = Field (\x c _ a2 -> c x a2) fst

class Field2 r a x y | r x -> y, r y -> x, r -> a, x y -> r where
  f_2 :: Field r a x y

instance Field2 ( a, b ) b (a1 -> b -> ( a, b )) (a1 -> X -> ( a, b )) where
  f_2 = Field (\x c a1 _ -> c a1 x) snd
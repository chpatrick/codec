module Foreign.Codec
  ( ForeignContext, ForeignCodec, ForeignCodec'
    , peekCodec, pokeCodec, field
    ) where

import Control.Monad.Reader
import Foreign

import Data.Codec.Codec

type ForeignContext a = ReaderT (Ptr a) IO
-- | A foreign codec for @a@ given a pointer to @p@.
type ForeignCodec' p a = Codec (ForeignContext p) (ForeignContext p) a
-- | A foreign codec for @a@ given a pointer to itself.
type ForeignCodec a = ForeignCodec' a a

-- | Peek a value using a `ForeignCodec'`.
peekCodec :: ForeignCodec' p a -> Ptr p -> IO a
peekCodec (Codec r _)
  = runReaderT r

-- | Poke a value using a `ForeignCodec'`.
pokeCodec :: ForeignCodec' p a -> Ptr p -> a -> IO ()
pokeCodec (Codec _ w) ptr x
  = runReaderT (w x) ptr

-- | A codec for a field of a foreign structure, given its byte offset.
-- You can get an offset easily using @{#offset struct_type, field}@ with @hsc2hs@.
field :: Storable a => Int -> ForeignCodec' p a
field off = Codec
  { parse = ReaderT (`peekByteOff` off)
  , produce = \x -> ReaderT $ \ptr -> pokeByteOff ptr off x
}

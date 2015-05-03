module Foreign.Codec
  ( -- * Foreign codecs
  ForeignContext, ForeignCodec, ForeignCodec'
  , peekWith, pokeWith, fieldWith, field
  ) where

import Control.Monad.Reader
import Data.Default.Class
import Foreign

import Data.Codec.Codec

type ForeignContext a = ReaderT (Ptr a) IO
-- | A foreign codec for @a@ given a pointer to @p@.
type ForeignCodec' p a = Codec (ForeignContext p) (ForeignContext p) a
-- | A foreign codec for @a@ given a pointer to itself.
-- Use `def` from `Default` to get a codec that uses a `Storable` instance,
type ForeignCodec a = ForeignCodec' a a

-- | Peek a value using a `ForeignCodec'`.
peekWith :: ForeignCodec' p a -> Ptr p -> IO a
peekWith (Codec r _)
  = runReaderT r

-- | Poke a value using a `ForeignCodec'`.
pokeWith :: ForeignCodec' p a -> Ptr p -> a -> IO ()
pokeWith (Codec _ w) ptr x
  = runReaderT (w x) ptr

instance Storable a => Default (ForeignCodec a) where
  def = Codec (ReaderT peek) (\x -> ReaderT (`poke`x))

-- | A codec for a field of a foreign structure, given its byte offset and a sub-codec.
-- You can get an offset easily using @{#offset struct_type, field}@ with @hsc2hs@.
fieldWith :: ForeignCodec a -> Int -> ForeignCodec' p a
fieldWith cd off = Codec
  { parse = inField $ parse cd
  , produce = inField . produce cd
  } where inField = withReaderT (`plusPtr` off)

-- | A codec for a field of a foreign structure, using the field's `Storable` instance.
field :: Storable a => Int -> ForeignCodec' p a
field = fieldWith def
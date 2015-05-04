module Foreign.Codec
  ( -- * Foreign codecs
  ForeignContext, ForeignCodec, ForeignCodec'
  , peekWith, pokeWith, storable, field
  , cNum, cInt, cBool
  ) where

import Control.Monad.Reader
import Foreign
import Foreign.C

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

-- | A codec for a field of a foreign structure, given its byte offset and a sub-codec.
-- You can get an offset easily using @{#offset struct_type, field}@ with @hsc2hs@.
field :: Int -> ForeignCodec' f a -> ForeignCodec' p a
field off cd = Codec
  { parse = inField $ parse cd
  , produce = inField . produce cd
  } where inField = withReaderT (`plusPtr` off)

-- | A `ForeignCodec` for any `Storable` type.
storable :: Storable a => ForeignCodec a
storable = Codec (ReaderT peek) (\x -> ReaderT (`poke`x))

-- | Store any integral type.
cNum :: (Integral c, Storable c, Integral a) => ForeignCodec' c a
cNum = mapCodec fromIntegral fromIntegral storable

cInt :: Integral a => ForeignCodec' CInt a
cInt = cNum

cBool :: ForeignCodec Bool
cBool = storable
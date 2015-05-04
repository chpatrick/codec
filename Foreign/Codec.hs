module Foreign.Codec
  ( -- * Foreign codecs
  ForeignContext, ForeignCodec, ForeignCodec'
  , peekWith, pokeWith, storable, field
  , cast, cBool
  , codecFor
  ) where

import Control.Monad.Reader
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

castContext :: ForeignCodec' c a -> ForeignCodec' c' a
castContext = mapCodecF castc castc
  where castc = withReaderT castPtr

-- | Store any integral type.
cast :: (Integral c, Storable c, Integral a) => ForeignCodec' c a
cast = mapCodec fromIntegral fromIntegral storable

-- | Store a `Bool` in any `Integral` `Ptr`.
cBool :: Integral c => ForeignCodec' c Bool
cBool = castContext storable

-- | Restrict the pointer type of a given codec. Utility function for the @numField@ macro.
codecFor :: c -> ForeignCodec' c a -> ForeignCodec' c a
codecFor _ = id
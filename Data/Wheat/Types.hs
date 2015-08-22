{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Wheat is based around bidirectional codecs, although they are not
-- constrained to deocde to the same type they decode (see
-- 'Codec''). For the common case, these two type parameters will be
-- the same.
--
-- Decoders are Monads, this means we can construct a decoder for a
-- larger type out of decoders for smaller types along with a function
-- to combine the parts. Encoders are Divisibles, this means we can
-- construct an encoder for a larger type out of encoders for smaller
-- types along with a function to split the whole.
module Data.Wheat.Types where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Data.Data
import Data.Foldable
import Data.Functor.Contravariant
import Data.Monoid
import Data.Traversable
import GHC.Generics

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L

-- * Codecs

-- | A codec where the type we encode is the same as the type we
-- decode to. Most of the primitive codec functions will have this
-- type.
type Codec a = Codec' a a

-- | A more general codec where we are covariant in the type we encode
-- but contravariant in the type we decode.
--
-- There are two constructors,
--
-- - @Plain@: The simplest type of codec is just a pair of a 'Decoder'
--   and 'Encoder'.
--
-- - @Wrap@: Sometimes it's useful to operate in terms of one type,
--   but do transformation to another under the hood (see the
--   bencoding example in the README), as this means we can have a
--   collection of codecs which are conceptually the same type, but
--   have very different implementations.
type Codec' d e = GCodec L.ByteString B.Builder d e
data GCodec i b d e where
  Plain :: GDecoder i d -> GEncoder b e -> GCodec i b d e
  Wrap  :: (d' -> Maybe d) -> (e -> Maybe e') -> GCodec i b d' e' -> GCodec i b d e

-- * Decoders

-- | A decoder is a function which, given some input bytestring, will
-- attempt to decode it according to the construction of the decoder
-- and return any remaining input.
type Decoder    a = GDecoder L.ByteString a
type GDecoder i a = StateT i Maybe a

-- | Construct a decoder.
toDecoder :: (i -> Maybe (a, i)) -> GDecoder i a
toDecoder = StateT

-- | Run a decoder.
runDecoder :: GDecoder i a -> i -> Maybe (a, i)
runDecoder = runStateT

-- | Run a codec's decoder.
decode :: i -> GCodec i b d e -> Maybe d
decode b c = fst <$> runDecoder (decoderOf c) b

-- | Get the decoder of a codec.
decoderOf :: GCodec i b d e -> GDecoder i d
decoderOf (Plain decoder _) = decoder
decoderOf (Wrap df _ c) = toDecoder $ \b -> case runDecoder (decoderOf c) b of
  Just (d, b') -> (\d' -> (d',b')) <$> df d
  Nothing -> Nothing

-- * Encoders

-- | An encoder is a function which, given some input value, will
-- attempt to encode it according to the construction of the encoder,
-- producing a ByteString builder.
type Encoder    a = GEncoder B.Builder a
type GEncoder b a = Op (Both b) a

-- | Construct an encoder.
toEncoder :: (a -> Maybe b) -> GEncoder b a
toEncoder f = Op $ Both . f

-- | Run a decoder.
runEncoder :: GEncoder b a -> a -> Maybe b
runEncoder e = getBoth . getOp e

-- | Run a codec's encoder.
encode :: e -> GCodec i b d e -> Maybe b
encode a c = runEncoder (encoderOf c) a

-- | Get the encoder of a codec.
encoderOf :: GCodec i b d e -> GEncoder b e
encoderOf (Plain _ encoder) = encoder
encoderOf (Wrap _ ef c) = toEncoder $ ef >=> runEncoder (encoderOf c)

-- * 'Both' Monoid

-- |The 'Both' Monoid is like 'Maybe', but requires both of its
-- arguments to be 'Just' or the result will be 'Nothing'.
newtype Both a = Both { getBoth :: Maybe a }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, Generic1, Functor, Applicative, Alternative, Monad, MonadPlus, Foldable, Traversable)

instance Monoid a => Monoid (Both a) where
  mempty = Both $ Just mempty
  mappend (Both (Just x)) (Both (Just y)) = Both . Just $ x <> y
  mappend _ _ = Both Nothing

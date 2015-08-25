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

-- For codecs
import Control.Applicative ((<$>), (<|>))
import Data.Functor.Contravariant ((>$<))
import Data.Monoid (Monoid(..))
import Data.Profunctor (Profunctor(..))

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Semigroup as G

-- For decoders
import Data.Both (Both(..))
import Control.Monad.Trans.State (StateT(..))

-- For encoders
import Data.Functor.Contravariant (Op(..))

-- * Codecs

-- | A codec where the type we encode is the same as the type we
-- decode to. Most of the primitive codec functions will have this
-- type.
type Codec a = Codec' a a

-- | A more general codec where we are contravariant in the type we
-- encode but covariant in the type we decode.
--
-- The Semigroup and Monoid instances represent left-biased choice. If
-- the left codec fails, the right will be tried.
type Codec'     e d = GCodec L.ByteString B.Builder e d
data GCodec i b e d = Codec { encoderOf :: GEncoder b e, decoderOf :: GDecoder i d }

instance Profunctor (GCodec i b) where
  lmap f c = c { encoderOf = f >$< encoderOf c }
  rmap f c = c { decoderOf = f <$> decoderOf c }

instance G.Semigroup (GCodec i b e d) where
  this <> other = Codec (toEncoder encodes) (toDecoder decodes) where
    decodes b = runDecoder (decoderOf this) b <|> runDecoder (decoderOf other) b
    encodes e = runEncoder (encoderOf this) e <|> runEncoder (encoderOf other) e

-- | 'mempty' is the codec which always fails.
instance Monoid (GCodec i b e d) where
  mappend = (G.<>)
  mempty  = Codec (toEncoder $ const Nothing) (toDecoder $ const Nothing)

-- * Decoders

-- | A decoder is a function which, given some input bytestring, will
-- attempt to decode it according to the construction of the decoder
-- and return any remaining input.
type Decoder    a = GDecoder L.ByteString a
type GDecoder i a = StateT i Both a

-- | Construct a decoder.
toDecoder :: (i -> Maybe (a, i)) -> GDecoder i a
toDecoder f = toDecoder' $ Both . f

-- | Variant of 'toDecoder'.
toDecoder' :: (i -> Both (a, i)) -> GDecoder i a
toDecoder' = StateT

-- | Run a decoder.
runDecoder :: GDecoder i a -> i -> Maybe (a, i)
runDecoder d = getBoth . runDecoder' d

-- | Variant of 'runDecoder'
runDecoder' :: GDecoder i a -> i -> Both (a, i)
runDecoder' = runStateT

-- | Run a codec's decoder.
decode :: GCodec i b e d -> i -> Maybe d
decode c = fmap fst . runDecoder (decoderOf c)

-- * Encoders

-- | An encoder is a function which, given some input value, will
-- attempt to encode it according to the construction of the encoder,
-- producing a ByteString builder.
type Encoder    a = GEncoder B.Builder a
type GEncoder b a = Op (Both b) a

-- | Construct an encoder.
toEncoder :: (a -> Maybe b) -> GEncoder b a
toEncoder f = toEncoder' $ Both . f

-- | Variant of 'toEncoder'.
toEncoder' :: (a -> Both b) -> GEncoder b a
toEncoder' = Op

-- | Run an encoder.
runEncoder :: GEncoder b a -> a -> Maybe b
runEncoder e = getBoth . runEncoder' e

-- | Variant of 'runEncoder'.
runEncoder' :: GEncoder b a -> a -> Both b
runEncoder' = getOp

-- | Run a codec's encoder.
encode :: GCodec i b e d -> e -> Maybe b
encode c = runEncoder $ encoderOf c

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

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Monoid
import Data.Void

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L

-- * Codecs

-- | A codec where the type we encode is the same as the type we
-- decode to. Most of the primitive codec functions will have this
-- type.
type Codec a = Codec' a a

-- | A more general codec where we are covariant in the type we decode
-- but contravariant in the type we encode.
type Codec' a b = (Decoder a, Encoder b)

-- * Decoders

-- | A decoder is a function which, given some input bytestring, will
-- attempt to decode it according to the construction of the decoder.
newtype Decoder a = Decoder { runDecoder :: L.ByteString -> Maybe a }

instance Functor Decoder where
  fmap f d = Decoder $ \b -> f <$> runDecoder d b

instance Applicative Decoder where
  pure = Decoder . const . Just

  df <*> da = Decoder $ \b -> runDecoder df b <*> runDecoder da b

instance Monad Decoder where
  return = pure

  d >>= f = Decoder $ \b -> case runDecoder d b of
    Just a  -> runDecoder (f a) b
    Nothing -> Nothing

-- | Run a codec's decoder.
decode :: L.ByteString -> Codec' a b -> Maybe a
decode b (d, _) = runDecoder d b

-- * Encoders

-- | An encoder is a function which, given some input value, will
-- encode it according to the construction of the encoder, producing a
-- ByteString builder.
newtype Encoder a = Encoder { runEncoder :: a -> B.Builder }

instance Contravariant Encoder where
  contramap f e = Encoder $ runEncoder e . f

instance Divisible Encoder where
  divide split fb fc = Encoder $ \a ->
    let (b, c) = split a
     in runEncoder fb b <> runEncoder fc c

  conquer = Encoder $ const mempty

instance Decidable Encoder where
  choose f fb fc = Encoder $ either (runEncoder fb) (runEncoder fc) . f

  lose f = Encoder $ absurd . f

-- | Run a codec's encoder.
encode :: b -> Codec' a b -> B.Builder
encode a (_, e) = runEncoder e a

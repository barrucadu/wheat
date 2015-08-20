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
import Control.Arrow
import Control.Monad
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Maybe
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
type Codec' d e = (Decoder d, Encoder e)

-- * Decoders

-- | A decoder is a function which, given some input bytestring, will
-- attempt to decode it according to the construction of the decoder
-- and return any remaining input.
newtype Decoder a = Decoder { runDecoder :: L.ByteString -> Maybe (a, L.ByteString) }

instance Functor Decoder where
  fmap f d = Decoder $ \b -> first f <$> runDecoder d b

instance Applicative Decoder where
  pure a = Decoder . const $ Just (a, L.empty)

  df <*> da = Decoder $ \b -> case runDecoder df b of
    Just (f, b') -> first f <$> runDecoder da b'
    Nothing -> Nothing

instance Alternative Decoder where
  empty = Decoder $ const Nothing

  da <|> db = Decoder $ \b -> fromMaybe (runDecoder db b) (Just <$> runDecoder da b)

instance Monad Decoder where
  return = pure

  d >>= f = Decoder $ \b -> case runDecoder d b of
    Just (a, b') -> runDecoder (f a) b'
    Nothing      -> Nothing

instance MonadPlus Decoder where
  mzero = empty
  mplus = (<|>)

-- | Run a codec's decoder.
decode :: L.ByteString -> Codec' d e -> Maybe d
decode b c = fst <$> runDecoder (decoderOf c) b

-- | Get the decoder of a codec.
decoderOf :: Codec' d e -> Decoder d
decoderOf = fst

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

instance Monoid (Encoder a) where
  mempty = Encoder $ const mempty
  mappend e1 e2 = Encoder $ \a -> runEncoder e1 a <> runEncoder e2 a

-- | Run a codec's encoder.
encode :: e -> Codec' d e -> B.Builder
encode a c = runEncoder (encoderOf c) a

-- | Get the encoder of a codec.
encoderOf :: Codec' d e -> Encoder e
encoderOf = snd

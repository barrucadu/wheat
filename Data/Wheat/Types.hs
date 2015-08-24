{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
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

-- For codecs
import Control.Applicative ((<$>), (<|>))
import Data.Functor.Contravariant ((>$<))
import Data.Monoid (Monoid(..))
import Data.Profunctor (Profunctor(..))

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Semigroup as G

-- For decoders
import Control.Monad.Trans.State (StateT(..))

-- For encoders
import Data.Functor.Contravariant (Op(..))

-- For the Both type
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Data (Data(..))
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Typeable (Typeable)
import GHC.Generics (Generic(..), Generic1(..))

-- * Codecs

-- | A codec where the type we encode is the same as the type we
-- decode to. Most of the primitive codec functions will have this
-- type.
type Codec a = Codec' a a

-- | A more general codec where we are covariant in the type we encode
-- but contravariant in the type we decode.
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
toDecoder f = StateT $ Both . f

-- | Run a decoder.
runDecoder :: GDecoder i a -> i -> Maybe (a, i)
runDecoder d = getBoth . runStateT d

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
toEncoder f = Op $ Both . f

-- | Run a decoder.
runEncoder :: GEncoder b a -> a -> Maybe b
runEncoder e = getBoth . getOp e

-- | Run a codec's encoder.
encode :: GCodec i b e d -> e -> Maybe b
encode c = runEncoder $ encoderOf c

-- * 'Both' Monoid

-- |The 'Both' Monoid is like 'Maybe', but requires both of its
-- arguments to be 'Just' or the result will be 'Nothing'.
newtype Both a = Both { getBoth :: Maybe a }
  deriving (Eq, Ord, Read, Show, Data, Typeable, Generic, Generic1, Functor, Applicative, Alternative, Monad, MonadPlus, Foldable, Traversable)

instance Monoid a => Monoid (Both a) where
  mempty = Both $ Just mempty
  mappend (Both (Just x)) (Both (Just y)) = Both . Just $ x `mappend` y
  mappend _ _ = Both Nothing

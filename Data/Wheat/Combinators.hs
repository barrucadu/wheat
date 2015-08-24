-- | A collection of commonly-used combinators to build up larger
-- codecs.
module Data.Wheat.Combinators where

import Control.Applicative ((<$>))
import Control.Monad ((>=>))
import Data.Functor.Contravariant.Divisible (divide)
import Data.Monoid (Monoid, (<>), mempty)

-- Local imports
import Data.Wheat.Types

-- | Sequential composition of codecs.
--
-- When encoding, the input value is encoded with both codecs and the
-- results are concatenated. When decoding, the remaining bytestring
-- from the first codec is used as input to the second.
(<:>) :: Monoid b => GCodec i b e d1 -> GCodec i b e d2 -> GCodec i b e (d1, d2)
c1 <:> c2 = Codec (encoderOf c1 <> encoderOf c2) decoder where
  decoder = do
    x <- decoderOf c1
    y <- decoderOf c2
    return (x, y)

-- | Sequential composition of codecs, combining the results of
-- decoding monoidally.
(<<:>>) :: (Monoid b, Monoid d) => GCodec i b e d -> GCodec i b e d -> GCodec i b e d
c1 <<:>> c2 = Codec (encoderOf cx) (uncurry (<>) <$> decoderOf cx) where
  cx = c1 <:> c2

-- | Right-biased sequential composition of codecs.
--
-- Encoding behaviour is the same as '<:>', decoding throws away the
-- result of the first codec.
(<:>>) :: Monoid b => GCodec i b e d1 -> GCodec i b e d2 -> GCodec i b e d2
c1 <:>> c2 = Codec (encoderOf cx) (snd <$> decoderOf cx) where
  cx = c1 <:> c2

-- | Left-biased sequential composition of codecs.
--
-- Encoding behaviour is the same as '<:>', decoding throws away the
-- result of the second codec.
(<<:>) :: Monoid b => GCodec i b e d1 -> GCodec i b e d2 -> GCodec i b e d1
c1 <<:> c2 = Codec (encoderOf cx) (fst <$> decoderOf cx) where
  cx = c1 <:> c2

-- | Combine two codecs into a codec for tuples of those types, where
-- the encoded forms of each element are concatenated.
(<++>) :: Monoid b => GCodec i b e1 d1 -> GCodec i b e2 d2 -> GCodec i b (e1, e2) (d1, d2)
(<++>) = separate id id

-- | Lift a codec over a type to over a list of that type.
--
-- Encoded elements are concatenated. Decoding never fails, as an
-- empty list is acceptable.
elementwise :: Monoid b => GCodec i b e d -> GCodec i b [e] [d]
elementwise c = Codec encoder decoder where
  encoder = toEncoder go where
    go = getBoth . foldr ((<>) . Both . runEncoder (encoderOf c)) mempty

  decoder = toDecoder $ go [] where
    go ds b = case runDecoder (decoderOf c) b of
      Just (d, b') -> go (d:ds) b'
      Nothing -> Just (reverse ds, b)

-- | Encode a value as a combination of header and encoded value. The
-- codec used for encoding/decoding the value itself receives the
-- (encoded/decoded) header as a parameter.
header :: Monoid b => (e -> h) -> GCodec i b h h -> (h -> GCodec i b e d) -> GCodec i b e d
header hf hc cf = Codec encoder decoder where
  encoder = toEncoder $ \e -> getBoth $
    let h = hf e
     in Both (runEncoder (encoderOf hc) h) <> Both (runEncoder (encoderOf $ cf h) e)

  decoder = decoderOf hc >>= decoderOf . cf

-- | Apply a divide-and-conquer approach: given a function to split up
-- the encoding into two smaller components, and a function to combine
-- the smaller components after decoding, construct a codec for the
-- more complex type.
separate :: Monoid b => (e -> (e1, e2)) -> ((d1, d2) -> d) -> GCodec i b e1 d1 -> GCodec i b e2 d2 -> GCodec i b e d
separate split merge c1 c2 = Codec encoder decoder where
  encoder = divide split (encoderOf c1) (encoderOf c2)
  decoder = do
    x <- decoderOf c1
    y <- decoderOf c2
    return $ merge (x, y)

-- | Given functions to convert the types, wrap one codec inside another.
--
-- Encoding and decoding will fail if the function does, even if the
-- inner codec succeeds.
wrap  :: (d' -> Maybe d) -> (e -> Maybe e') -> GCodec i b e' d' -> GCodec i b e d
wrap df ef c = Codec encoder decoder where
  encoder = toEncoder $ ef >=> runEncoder (encoderOf c)
  decoder = toDecoder $ \b -> case runDecoder (decoderOf c) b of
    Just (d, b') -> (\d' -> (d',b')) <$> df d
    Nothing -> Nothing

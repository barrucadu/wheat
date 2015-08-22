{-# LANGUAGE GADTs #-}
-- | A collection of commonly-used combinators to build up larger
-- codecs.
module Data.Wheat.Combinators where

import Control.Applicative
import Data.Functor.Contravariant.Divisible
import Data.Monoid
import Data.Wheat.Types

-- * Combinators

-- | Prioritised choice between codecs. If the left one fails the
-- right one will be tried.
(<||>) :: GCodec i b d e -> GCodec i b d e -> GCodec i b d e
this <||> other = Plain (toDecoder decodes) (toEncoder encodes) where
  decodes b = runDecoder (decoderOf other) b <|> runDecoder (decoderOf this) b
  encodes e = runEncoder (encoderOf other) e <|> runEncoder (encoderOf this) e

-- | Sequential composition of codecs.
--
-- When encoding, the input value is encoded with both codecs and the
-- results are concatenated. When decoding, the remaining bytestring
-- from the first codec is used as input to the second.
(<:>) :: Monoid b => GCodec i b d1 e -> GCodec i b d2 e -> GCodec i b (d1, d2) e
c1 <:> c2 = Plain decoder (encoderOf c1 <> encoderOf c2) where
  decoder = toDecoder $ \b -> do
    (x, b')  <- runDecoder (decoderOf c1) b
    (y, b'') <- runDecoder (decoderOf c2) b'
    Just ((x, y), b'')

-- | Sequential composition of codecs, combining the results of
-- decoding monoidally.
(<<:>>) :: (Monoid b, Monoid d) => GCodec i b d e -> GCodec i b d e -> GCodec i b d e
c1 <<:>> c2 = Plain (uncurry (<>) <$> decoderOf cx) (encoderOf cx) where
  cx = c1 <:> c2

-- | Right-biased sequential composition of codecs.
--
-- Encoding behaviour is the same as '<:>', decoding throws away the
-- result of the first codec.
(<:>>) :: Monoid b => GCodec i b d1 e -> GCodec i b d2 e -> GCodec i b d2 e
c1 <:>> c2 = Plain (snd <$> decoderOf cx) (encoderOf cx) where
  cx = c1 <:> c2

-- | Left-biased sequential composition of codecs.
--
-- Encoding behaviour is the same as '<:>', decoding throws away the
-- result of the second codec.
(<<:>) :: Monoid b => GCodec i b d1 e -> GCodec i b d2 e -> GCodec i b d1 e
c1 <<:> c2 = Plain (fst <$> decoderOf cx) (encoderOf cx) where
  cx = c1 <:> c2

-- | Combine two codecs into a codec for tuples of those types, where
-- the encoded forms of each element are concatenated.
(<++>) :: Monoid b => GCodec i b d1 e1 -> GCodec i b d2 e2 -> GCodec i b (d1, d2) (e1, e2)
(<++>) = separate id id

-- | Lift a codec over a type to over a list of that type.
--
-- Encoded elements are concatenated. Decoding never fails, as an
-- empty list is acceptable.
elementwise :: Monoid b => GCodec i b d e -> GCodec i b [d] [e]
elementwise c = Plain decoder encoder where
  decoder = toDecoder $ go [] where
    go ds b = case runDecoder (decoderOf c) b of
      Just (d, b') -> go (d:ds) b'
      Nothing -> Just (reverse ds, b)

  encoder = toEncoder go where
    go (e:es) = case runEncoder (encoderOf c) e of
      Just b -> case go es of
        Just b' -> Just $ b <> b'
        Nothing -> Nothing
      Nothing -> Nothing
    go [] = Just mempty

-- | Encode a value as a combination of header and encoded value. The
-- codec used for encoding/decoding the value itself receives the
-- (encoded/decoded) header as a parameter.
header :: Monoid b => (e -> h) -> GCodec i b h h -> (h -> GCodec i b d e) -> GCodec i b d e
header hf hc cf = Plain decoder encoder where
  decoder = toDecoder $ \b -> do
    (h, b')  <- runDecoder (decoderOf hc) b
    (x, b'') <- runDecoder (decoderOf $ cf h) b'
    Just (x, b'')

  encoder = toEncoder $ \e ->
    let h = hf e
     in runEncoder (encoderOf hc) h <> runEncoder (encoderOf $ cf h) e

-- | Apply a divide-and-conquer approach: given a function to split up
-- the encoding into two smaller components, and a function to combine
-- the smaller components after decoding, construct a codec for the
-- more complex type.
separate :: Monoid b => (e -> (e1, e2)) -> ((d1, d2) -> d) -> GCodec i b d1 e1 -> GCodec i b d2 e2 -> GCodec i b d e
separate split merge c1 c2 = Plain decoder encoder where
  decoder = toDecoder $ \b -> do
    (x, b')  <- runDecoder (decoderOf c1) b
    (y, b'') <- runDecoder (decoderOf c2) b'
    Just (merge (x, y), b'')

  encoder = divide split (encoderOf c1) (encoderOf c2)

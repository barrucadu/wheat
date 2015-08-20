-- | A collection of commonly-used basic codecs and combinators for
-- constructing larger codecs.
module Data.Wheat
  ( -- * Re-exported types
    module Data.Wheat.Types

    -- * ByteStrings
  , byteString
  , lazyByteString
  , constant
  , lazyConstant

  -- * Lists
  , elementwise

  -- * Numbers
  , asciiDigits

    -- * Combinators
  , (<:>), (<<:>>)
  , (<:>>), (<<:>)
  , (<++>)
  , dispatch
  , separate
  ) where

import Data.Foldable
import Data.Functor.Contravariant.Divisible
import Data.Monoid
import Data.Wheat.Types

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- * ByteStrings

-- | Encode a ByteString (the identity codec)
byteString :: Codec S.ByteString
byteString = (Decoder $ \b -> Just (L.toStrict b, L.empty), Encoder B.byteString)

-- | Encode a lazy ByteString (the identity codec)
lazyByteString :: Codec L.ByteString
lazyByteString = (Decoder $ \b -> Just (b, L.empty), Encoder B.lazyByteString)

-- | Encode a constant ByteString.
--
-- Decoding will fail if the supplied ByteString is not a prefix of
-- the actual one. Encoding will ignore its argument.
constant :: S.ByteString -> Codec' S.ByteString e
constant c = (Decoder check, Encoder . const $ B.byteString c) where
  check bs =
    let (pref, suff) = L.splitAt (fromIntegral $ S.length c) bs
     in if L.fromStrict c == pref
        then Just (c, suff)
        else Nothing

-- | Encode a constant lazy ByteString.
--
-- Decoding will fail if the actual ByteString differs from the
-- supplied one. Encoding will ignore its argument.
lazyConstant :: L.ByteString -> Codec' L.ByteString e
lazyConstant c = (Decoder check, Encoder . const $ B.lazyByteString c) where
  check bs =
    let (pref, suff) = L.splitAt (L.length c) bs
     in if c == pref
        then Just (c, suff)
        else Nothing

-- * Lists

-- | Lift a codec over a type to over a list of that type.
--
-- Encoded elements are concatenated. Decoding never fails, as an
-- empty list is acceptable.
elementwise :: Codec' d e -> Codec' [d] [e]
elementwise (decoder, encoder) = (Decoder $ decodes [], Encoder encodes) where
  decodes ds b = case runDecoder decoder b of
    Just (d, b') -> decodes (d:ds) b'
    Nothing -> Just (reverse ds, b)

  encodes = foldMap $ runEncoder encoder

-- * Numbers

-- | Encode an Int as ASCII digits.
--
-- Decoding will consume as many bytes from the start of the input as
-- represent ASCII digits, failing only if no bytes do.
asciiDigits :: Codec Int
asciiDigits = (Decoder go, Encoder B.intDec) where
  go bs =
    let (digits, rest) = L.span isAsciiDigit bs
     in if L.length digits == 0
        then Nothing
        else Just (foldl' (\acc w -> 10 * acc + fromAsciiDigit w) 0 $ L.unpack digits, rest)

  isAsciiDigit   w = w >= 48 && w <= 57
  fromAsciiDigit w = fromIntegral w - 48

-- * Combinators

-- | Sequential composition of codecs.
--
-- When encoding, the input value is encoded with both codecs and the
-- results are concatenated. When decoding, the remaining bytestring
-- from the first codec is used as input to the second.
(<:>) :: Codec' d1 e -> Codec' d2 e -> Codec' (d1, d2) e
(d1, e1) <:> (d2, e2) = (decoder, e1 <> e2) where
  decoder = Decoder $ \b -> do
    (x, b')  <- runDecoder d1 b
    (y, b'') <- runDecoder d2 b'
    Just ((x, y), b'')

-- | Sequential composition of codecs, combining the results of
-- decoding monoidally.
(<<:>>) :: Monoid d => Codec' d e -> Codec' d e -> Codec' d e
c1 <<:>> c2 = (uncurry (<>) <$> decoder, encoder) where
  (decoder, encoder) = c1 <:> c2

-- | Right-biased sequential composition of codecs.
--
-- Encoding behaviour is the same as '<:>', decoding throws away the
-- result of the first codec.
(<:>>) :: Codec' d1 e -> Codec' d2 e -> Codec' d2 e
c1 <:>> c2 = (snd <$> decoder, encoder) where
  (decoder, encoder) = c1 <:> c2

-- | Left-biased sequential composition of codecs.
--
-- Encoding behaviour is the same as '<:>', decoding throws away the
-- result of the second codec.
(<<:>) :: Codec' d1 e -> Codec' d2 e -> Codec' d1 e
c1 <<:> c2 = (fst <$> decoder, encoder) where
  (decoder, encoder) = c1 <:> c2

-- | Combine two codecs into a codec for tuples of those types, where
-- the encoded forms of each element are concatenated.
(<++>) :: Codec' d1 e1 -> Codec' d2 e2 -> Codec' (d1, d2) (e1, e2)
(<++>) = separate id id

-- | Choose an encoder based on the actual value being encoded, with a
-- decoder.
--
-- Be careful that this codec actually does express a reversible
-- encoding when you have @d == e@!
dispatch :: Decoder d -> (e -> B.Builder) -> Codec' d e
dispatch decoder encoderf = (decoder, Encoder encoderf)

-- | Apply a divide-and-conquer approach: given a function to split up
-- the encoding into two smaller components, and a function to combine
-- the smaller components after decoding, construct a codec for the
-- more complex type.
separate :: (e -> (e1, e2)) -> ((d1, d2) -> d) -> Codec' d1 e1 -> Codec' d2 e2 -> Codec' d e
separate split merge (d1, e1) (d2, e2) = (decoder, divide split e1 e2) where
  decoder = Decoder $ \b -> do
    (x, b')  <- runDecoder d1 b
    (y, b'') <- runDecoder d2 b'
    Just (merge (x, y), b'')

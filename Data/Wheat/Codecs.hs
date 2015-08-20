-- | A collection of commonly-used codecs.
module Data.Wheat.Codecs where

import Control.Applicative
import Control.Arrow
import Data.Foldable
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

-- | Encode the first N bytes of a ByteString.
--
-- Decoding will fail if the actual ByteString is not at least as long
-- as the given length.
firstN :: Int -> Codec S.ByteString
firstN len = (Decoder check, Encoder $ B.byteString . S.take len) where
  check bs =
    let (pref, suff) = L.splitAt len' bs
    in if L.length pref == len'
       then Just (L.toStrict pref, suff)
       else Nothing

  len' = fromIntegral len

-- | Encode the first N bytes of a lazy ByteString.
--
-- Decoding will fail if the actual ByteString is not at least as long
-- as the given length.
lazyFirstN :: Int -> Codec L.ByteString
lazyFirstN len = (Decoder check, Encoder $ B.lazyByteString . L.take len') where
  check bs =
    let (pref, suff) = L.splitAt len' bs
    in if L.length pref == len'
       then Just (pref, suff)
       else Nothing

  len' = fromIntegral len

-- | Encode a ByteString using a collection of codewords, falling back
-- to the identity function when a codeword is missing. Both encoding
-- and decoding attempt to match codewords in order, allowing for
-- non-prefix-free codes.
codewords :: [(S.ByteString, S.ByteString)] -> Codec S.ByteString
codewords cws = (Decoder $ wrap . code decwords, Encoder $ code encwords . L.fromStrict) where
  wrap bldr = Just (L.toStrict $ B.toLazyByteString bldr, L.empty)

  code codes bs = case findCodeword codes bs of
    Just (p, c) -> B.lazyByteString p <> code codes (L.drop (L.length c) bs)
    Nothing
      | L.null bs -> mempty
      | otherwise ->
        let (hd, tl) = L.splitAt 1 bs
         in B.lazyByteString hd <> code codes tl

  findCodeword ((p,c):cs) bs
    | p `L.isPrefixOf` bs = Just (p, c)
    | otherwise = findCodeword cs bs
  findCodeword [] _ = Nothing

  encwords = [(L.fromStrict c, L.fromStrict p) | (p, c) <- cws, not $ S.null c]
  decwords = [(L.fromStrict p, L.fromStrict c) | (p, c) <- cws, not $ S.null p]

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
-- represent ASCII digits (with an optional single leading minus
-- sign), failing only if no bytes do.
asciiDigits :: Codec Int
asciiDigits = (Decoder go, Encoder B.intDec) where
  go bs = case L.splitAt 1 bs of
    (s, bs')
      | s == L.pack [45] -> first negate <$> getNum bs'
      | otherwise -> getNum bs

  getNum bs =
    let (digits, rest) = L.span isAsciiDigit bs
     in if L.length digits == 0
        then Nothing
        else Just (foldl' (\acc w -> 10 * acc + fromAsciiDigit w) 0 $ L.unpack digits, rest)

  isAsciiDigit   w = w >= 48 && w <= 57
  fromAsciiDigit w = fromIntegral w - 48

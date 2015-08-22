-- | A collection of commonly-used codecs.
module Data.Wheat.Codecs where

import Control.Applicative
import Control.Arrow
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Wheat.Combinators
import Data.Wheat.Types

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- * ByteStrings

-- | Encode a ByteString (the identity codec)
byteString :: Codec S.ByteString
byteString = Plain decoder encoder where
  decoder = toDecoder $ \b -> Just (L.toStrict b, L.empty)
  encoder = toEncoder $ Just . B.byteString

-- | Encode a lazy ByteString (the identity codec)
lazyByteString :: Codec L.ByteString
lazyByteString = Plain decoder encoder where
  decoder = toDecoder $ \b -> Just (b, L.empty)
  encoder = toEncoder $ Just . B.lazyByteString

-- | Encode a constant ByteString.
--
-- Decoding will fail if the supplied ByteString is not a prefix of
-- the actual one. Encoding will ignore its argument.
constant :: S.ByteString -> Codec' S.ByteString e
constant c = Plain decoder encoder where
  decoder = toDecoder $ \bs ->
    let (pref, suff) = L.splitAt (fromIntegral $ S.length c) bs
     in if L.fromStrict c == pref
        then Just (c, suff)
        else Nothing

  encoder = toEncoder . const . Just $ B.byteString c

-- | Encode a constant lazy ByteString.
--
-- Decoding will fail if the actual ByteString differs from the
-- supplied one. Encoding will ignore its argument.
lazyConstant :: L.ByteString -> Codec' L.ByteString e
lazyConstant c = Plain decoder encoder where
  decoder = toDecoder $ \bs ->
    let (pref, suff) = L.splitAt (L.length c) bs
     in if c == pref
        then Just (c, suff)
        else Nothing

  encoder = toEncoder . const . Just $ B.lazyByteString c

-- | Encode the first N bytes of a ByteString.
--
-- Decoding will fail if the actual ByteString is not at least as long
-- as the given length.
firstN :: Int -> Codec S.ByteString
firstN len = Plain decoder encoder where
  decoder = toDecoder $ \bs ->
    let (pref, suff) = L.splitAt len' bs
    in if L.length pref == len'
       then Just (L.toStrict pref, suff)
       else Nothing

  encoder = toEncoder $ Just . B.byteString . S.take len

  len' = fromIntegral len

-- | Encode the first N bytes of a lazy ByteString.
--
-- Decoding will fail if the actual ByteString is not at least as long
-- as the given length.
lazyFirstN :: Int -> Codec L.ByteString
lazyFirstN len = Plain decoder encoder where
  decoder = toDecoder $ \bs ->
    let (pref, suff) = L.splitAt len' bs
    in if L.length pref == len'
       then Just (pref, suff)
       else Nothing

  encoder = toEncoder $ Just . B.lazyByteString . L.take len'

  len' = fromIntegral len

-- | Encode a ByteString using a collection of codewords, falling back
-- to the identity function when a codeword is missing. Both encoding
-- and decoding attempt to match codewords in order, allowing for
-- non-prefix-free codes.
codewords :: [(S.ByteString, S.ByteString)] -> Codec S.ByteString
codewords cws = Plain decoder encoder where
  decoder = toDecoder $ wrap . code decwords
  encoder = toEncoder $ Just . code encwords . L.fromStrict

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

-- * Numbers

-- | Encode an Int as ASCII digits.
--
-- Decoding will consume as many bytes from the start of the input as
-- represent ASCII digits (with an optional single leading minus
-- sign), failing only if no bytes do.
asciiDigits :: Codec Int
asciiDigits = Plain decoder encoder where
  decoder = toDecoder go where
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

  encoder = toEncoder $ Just . B.intDec

-- * Miscellaneous

-- | Surround a codec with delimiters.
--
-- If an ending delimiter is provided, and multiple instances of the
-- ending delimiter crop up in the actual ByteString when decoding,
-- the wrapped codec will be tried for all possible endings until
-- decoding succeeds. If decoding succeeds but does not consume all
-- the bytes between the starting and ending tokens, the overall
-- decoding will fail.
delimited :: Maybe S.ByteString -> Maybe S.ByteString -> Codec' d e -> Codec' d e
delimited pre post c = Plain decoder encoder where
  decoder = do 
    _ <- decoderOf $ constant pre'
    d <- toDecoder $ decoder' L.empty
    _ <- decoderOf $ constant post'
    return d

  decoder' prefix b =
    let (string, rest) = toDelimiter b
        string' = prefix <> string
     in case runDecoder (decoderOf c) string' of
          Just (d, b')
            | L.null b' -> Just (d, rest)
            | otherwise -> Nothing
          Nothing -> decoder' string' rest

  toDelimiter = go L.empty where
    go str rest
      | L.fromStrict post' `L.isPrefixOf` rest = (str, rest)
      | L.null rest = (str, L.empty)
      | otherwise = go (str <> L.take 1 rest) $ L.drop 1 rest

  encoder = encoderOf $ constant pre' <:>> c <<:> constant post'

  pre'  = fromMaybe S.empty pre
  post' = fromMaybe S.empty post

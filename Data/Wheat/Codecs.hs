-- | A collection of commonly-used codecs.
--
-- Codecs need to know the underlying concrete types they are dealing
-- with. These codecs are written to deal with
-- 'Data.ByteString.Lazy.ByteString' and
-- 'Data.ByteString.Builder.Builder'.
--
-- Codecs can be categorised in a few different ways:
--
--   [Greedy] A greedy codec may consume more of the input
--     'Data.ByteString.Lazy.ByteString' when decoding than it
--     produced when encoding. The 'byteString' codec is greedy: it
--     will consume all remaining input. Greedy codecs can be made
--     non-greedy by adding an end delimiter with the 'delimited'
--     codec.
--
--  [Reversible] A reversible codec has the property that when
--    specialised to type @Codec a@, we have as true
--
--    > \c a -> Just a == (encode c a >>= decode c . toLazyByteString)
--
--    Or, in words, encoding followed by decoding gives back the same
--    thing.
--
--  [Well-behaved] A well-behaved codec is both reversible and
--    non-greedy: you get out what you put in, no more and no less.
module Data.Wheat.Codecs where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Monoid ((<>), mempty)

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- Local imports
import Data.Wheat.Combinators
import Data.Wheat.Types

-- * ByteStrings

-- | Encode a ByteString (the identity codec)
--
-- This is a greedy codec.
byteString :: Codec S.ByteString
byteString = Codec encoder decoder where
  encoder = toEncoder $ Just . B.byteString
  decoder = toDecoder $ \b -> Just (L.toStrict b, L.empty)

-- | Encode a lazy ByteString (the identity codec)
--
-- This is a greedy codec.
lazyByteString :: Codec L.ByteString
lazyByteString = Codec encoder decoder where
  encoder = toEncoder $ Just . B.lazyByteString
  decoder = toDecoder $ \b -> Just (b, L.empty)

-- | Encode a constant ByteString.
--
-- Decoding will fail if the supplied ByteString is not a prefix of
-- the actual one. Encoding will ignore its argument.
--
-- This is not necessarily a reversible codec.
constant :: S.ByteString -> Codec' e S.ByteString
constant c = Codec encoder decoder where
  encoder = toEncoder . const . Just $ B.byteString c
  decoder = toDecoder $ \bs ->
    let (pref, suff) = L.splitAt (fromIntegral $ S.length c) bs
     in if L.fromStrict c == pref
        then Just (c, suff)
        else Nothing

-- | Encode a constant lazy ByteString.
--
-- Decoding will fail if the actual ByteString differs from the
-- supplied one. Encoding will ignore its argument.
--
-- This is not necessarily a reversible codec.
lazyConstant :: L.ByteString -> Codec' e L.ByteString
lazyConstant c = Codec encoder decoder where
  encoder = toEncoder . const . Just $ B.lazyByteString c
  decoder = toDecoder $ \bs ->
    let (pref, suff) = L.splitAt (L.length c) bs
     in if c == pref
        then Just (c, suff)
        else Nothing

-- | Encode the first N bytes of a ByteString.
--
-- Decoding will fail if the actual ByteString is not at least as long
-- as the given length.
--
-- This is not necessarily a reversible codec.
firstN :: Int -> Codec S.ByteString
firstN len = Codec encoder decoder where
  encoder = toEncoder $ Just . B.byteString . S.take len
  decoder = toDecoder $ \bs ->
    let (pref, suff) = L.splitAt len' bs
    in if L.length pref == len'
       then Just (L.toStrict pref, suff)
       else Nothing

  len' = fromIntegral len

-- | Encode the first N bytes of a lazy ByteString.
--
-- Decoding will fail if the actual ByteString is not at least as long
-- as the given length.
--
-- This is not necessarily a reversible codec.
lazyFirstN :: Int -> Codec L.ByteString
lazyFirstN len = Codec encoder decoder where
  encoder = toEncoder $ Just . B.lazyByteString . L.take len'
  decoder = toDecoder $ \bs ->
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
codewords cws = Codec encoder decoder where
  encoder = toEncoder $ Just . code encwords . L.fromStrict
  decoder = toDecoder $ wrap . code decwords

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
asciiDigits = Codec encoder decoder where
  encoder = toEncoder $ Just . B.intDec
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

-- * Miscellaneous

-- | Surround a codec with delimiters.
--
-- If an ending delimiter is provided, and multiple instances of the
-- ending delimiter crop up in the actual ByteString when decoding,
-- the wrapped codec will be tried for all possible endings until
-- decoding succeeds. If decoding succeeds but does not consume all
-- the bytes between the starting and ending tokens, the overall
-- decoding will fail.
--
-- This can be used to make a greedy codec non-greedy.
delimited :: Maybe S.ByteString -> Maybe S.ByteString -> Codec' e d -> Codec' e d
delimited pre post c = Codec encoder decoder where
  encoder = encoderOf $ constant pre' <:>> c <<:> constant post'
  decoder = do 
    void . decoderOf $ constant pre'
    d <- toDecoder $ decoder' L.empty
    void . decoderOf $ constant post'
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

  pre'  = fromMaybe S.empty pre
  post' = fromMaybe S.empty post

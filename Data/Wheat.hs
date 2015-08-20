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
  , firstN
  , lazyFirstN
  , codewords

  -- * Lists
  , elementwise

  -- * Numbers
  , asciiDigits

    -- * Combinators
  , (<:>), (<<:>>)
  , (<:>>), (<<:>)
  , (<++>)
  , delimited
  , dispatch
  , header
  , separate
  ) where

import Control.Applicative
import Control.Arrow
import Data.Foldable
import Data.Functor.Contravariant.Divisible
import Data.Maybe
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

-- | Surround a codec with delimiters.
--
-- If an ending delimiter is provided, and multiple instances of the
-- ending delimiter crop up in the actual ByteString when decoding,
-- the wrapped codec will be tried for all possible endings until
-- decoding succeeds. If decoding succeeds but does not consume all
-- the bytes between the starting and ending tokens, the overall
-- decoding will fail.
delimited :: Maybe S.ByteString -> Maybe S.ByteString -> Codec' d e -> Codec' d e
delimited pre post c = (decoder, encoder) where
  decoder = Decoder $ \b -> do
    (_, b')   <- runDecoder (decoderOf $ constant pre') b
    (d, b'')  <- runDecoder (Decoder $ decoder' L.empty) b'
    (_, b''') <- runDecoder (decoderOf $ constant post') b''
    Just (d, b''')

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

  (_, encoder) = constant pre' <:>> c <<:> constant post'

  pre'  = fromMaybe S.empty pre
  post' = fromMaybe S.empty post

-- | Choose an encoder based on the actual value being encoded, with a
-- decoder.
--
-- Be careful that this codec actually does express a reversible
-- encoding when you have @d == e@!
dispatch :: Decoder d -> (e -> B.Builder) -> Codec' d e
dispatch decoder encoderf = (decoder, Encoder encoderf)

-- | Encode a value as a combination of header and encoded value. The
-- codec used for encoding/decoding the value itself receives the
-- (encoded/decoded) header as a parameter.
header :: (e -> h) -> Codec h -> (h -> Codec' d e) -> Codec' d e
header hf (hd, he) cf = (decoder, encoder) where
  decoder = Decoder $ \b -> do
    (h, b')  <- runDecoder hd b
    (x, b'') <- runDecoder (decoderOf $ cf h) b'
    Just (x, b'')

  encoder = Encoder $ \e ->
    let h = hf e
     in runEncoder he h <> runEncoder (encoderOf $ cf h) e

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

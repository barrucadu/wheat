{-# LANGUAGE GADTs #-}
-- | A collection of commonly-used combinators to build up larger
-- codecs.
module Data.Wheat.Combinators where

import Control.Applicative
import Data.Functor.Contravariant.Divisible
import Data.Maybe
import Data.Monoid
import Data.Wheat.Codecs
import Data.Wheat.Types

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- * Combinators

-- | Prioritised choice between codecs. If the left one fails the
-- right one will be tried.
(<||>) :: Codec' d e -> Codec' d e -> Codec' d e
this <||> other = Plain (Decoder decodes) (Encoder encodes) where
  decodes b = runDecoder (decoderOf other) b <|> runDecoder (decoderOf this) b
  encodes e = runEncoder (encoderOf other) e <|> runEncoder (encoderOf this) e

-- | Sequential composition of codecs.
--
-- When encoding, the input value is encoded with both codecs and the
-- results are concatenated. When decoding, the remaining bytestring
-- from the first codec is used as input to the second.
(<:>) :: Codec' d1 e -> Codec' d2 e -> Codec' (d1, d2) e
c1 <:> c2 = Plain decoder (encoderOf c1 <> encoderOf c2) where
  decoder = Decoder $ \b -> do
    (x, b')  <- runDecoder (decoderOf c1) b
    (y, b'') <- runDecoder (decoderOf c2) b'
    Just ((x, y), b'')

-- | Sequential composition of codecs, combining the results of
-- decoding monoidally.
(<<:>>) :: Monoid d => Codec' d e -> Codec' d e -> Codec' d e
c1 <<:>> c2 = Plain (uncurry (<>) <$> decoderOf cx) (encoderOf cx) where
  cx = c1 <:> c2

-- | Right-biased sequential composition of codecs.
--
-- Encoding behaviour is the same as '<:>', decoding throws away the
-- result of the first codec.
(<:>>) :: Codec' d1 e -> Codec' d2 e -> Codec' d2 e
c1 <:>> c2 = Plain (snd <$> decoderOf cx) (encoderOf cx) where
  cx = c1 <:> c2

-- | Left-biased sequential composition of codecs.
--
-- Encoding behaviour is the same as '<:>', decoding throws away the
-- result of the second codec.
(<<:>) :: Codec' d1 e -> Codec' d2 e -> Codec' d1 e
c1 <<:> c2 = Plain (fst <$> decoderOf cx) (encoderOf cx) where
  cx = c1 <:> c2

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
delimited pre post c = Plain decoder encoder where
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

  encoder = encoderOf $ constant pre' <:>> c <<:> constant post'

  pre'  = fromMaybe S.empty pre
  post' = fromMaybe S.empty post

-- | Encode a value as a combination of header and encoded value. The
-- codec used for encoding/decoding the value itself receives the
-- (encoded/decoded) header as a parameter.
header :: (e -> h) -> Codec h -> (h -> Codec' d e) -> Codec' d e
header hf hc cf = Plain decoder encoder where
  decoder = Decoder $ \b -> do
    (h, b')  <- runDecoder (decoderOf hc) b
    (x, b'') <- runDecoder (decoderOf $ cf h) b'
    Just (x, b'')

  encoder = Encoder $ \e ->
    let h = hf e
     in runEncoder (encoderOf hc) h <> runEncoder (encoderOf $ cf h) e

-- | Apply a divide-and-conquer approach: given a function to split up
-- the encoding into two smaller components, and a function to combine
-- the smaller components after decoding, construct a codec for the
-- more complex type.
separate :: (e -> (e1, e2)) -> ((d1, d2) -> d) -> Codec' d1 e1 -> Codec' d2 e2 -> Codec' d e
separate split merge c1 c2 = Plain decoder encoder where
  decoder = Decoder $ \b -> do
    (x, b')  <- runDecoder (decoderOf c1) b
    (y, b'') <- runDecoder (decoderOf c2) b'
    Just (merge (x, y), b'')

  encoder = divide split (encoderOf c1) (encoderOf c2)

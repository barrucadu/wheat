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

  -- * Numbers
  , asciiDigits
  ) where

import Data.Foldable
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

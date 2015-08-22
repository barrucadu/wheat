module Utils where

import Data.ByteString.Builder
import Data.Wheat.Types

encDec :: Codec a -> a -> Maybe a
encDec c a = do
  b  <- encode c a
  a' <- decode c $ toLazyByteString b
  Just a'

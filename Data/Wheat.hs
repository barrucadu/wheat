-- | Wheat. a library for expressing reversible codecs for binary
-- serialisation.
module Data.Wheat
  ( -- * Codecs

  -- | A codec is conceptually a pair of an encoding function and a
  -- decoding function. Codecs need to know the concrete types they
  -- are dealing with: decoding uses lazy
  -- 'Data.ByteString.Lazy.ByteString' and encoding uses
  -- 'Data.ByteString.Builder.Builder', for efficiency reasons.

    module Data.Wheat.Codecs

  -- * Combinators

  -- | A combinator is a function for combining two or more codecs
  -- into a larger codec. Unlike codecs, a combinator does not need to
  -- know the concrete type being encoded to or decoded from, the same
  -- combinator can apply to any concrete types, whereas codecs would
  -- need to be rewritten.

  , module Data.Wheat.Combinators

  -- * Types

  -- | The codec types and associated functions. Each comes in two or
  -- three flavours: the basic ('Codec', 'Encoder', and 'Decoder')
  -- which is specialised to work with
  -- 'Data.ByteString.Lazy.ByteString' and
  -- 'Data.ByteString.Builder.Builder', and the generic ('GCodec',
  -- 'GEncoder', 'GDecoder') which are fully generic in the underlying
  -- concrete types, there is additionally a middle-ground codec type,
  -- 'Codec''. Most of the library is written in terms of the generic
  -- types, but users should be able to just use the more concrete
  -- ones and have everything just work.

  , module Data.Wheat.Types
  ) where

import Data.Wheat.Codecs
import Data.Wheat.Combinators
import Data.Wheat.Types

{-# ANN module ("HLint: ignore Use import/export shortcut" :: String) #-}
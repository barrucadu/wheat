wheat [![Build Status][build-status]][build-log]
=====

I decided to write a binary (de-)serialisation library based on
contravariant functors! More text will go here when this library
actually has a purpose and a niche.

The documentation of the latest developmental version is
[available online][docs].

Example: Bencoding
------------------

An implementation of Bencoding, a very simple encoding format commonly
used in bittorrent files.

There are four types of value:

- ByteStrings, encoded as `<length>:<contents>`
- Unbounded integers, encoded as `i<base 10 ascii>e`
- Lists, encoded as `l<contents>e`
- Dictionaries (with ByteString keys), encoded as `d<contents>e`,
  where the contents are encoded as `<encoded key><encoded value>`,
  with keys in lexicographical order.

Bencoding is simple and not terribly efficient, but it has its uses:
it is unaffected by endianness, and there is exactly one legal
encoding for a given data structure.

~~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}

module Data.Wheat.Bencode where

import Control.Applicative
import Data.ByteString
import Data.Functor.Contravariant
import Data.List
import Data.Ord
import Data.Wheat

import qualified Data.ByteString as S

data BValue =
    BInt Int
  | BBytes S.ByteString
  | BList [BValue]
  | BDict [(S.ByteString, BValue)]
  deriving (Eq, Read, Show)

bencode :: Codec BValue
bencode = dispatch decoder encoder where
  decoder = (BInt   <$> decoderOf bencodeInt)   <|>
            (BBytes <$> decoderOf bencodeBytes) <|>
            (BList  <$> decoderOf bencodeList)  <|>
            (BDict  <$> decoderOf bencodeDict)

  encoder (BInt   i) = encode i bencodeInt
  encoder (BBytes b) = encode b bencodeBytes
  encoder (BList  l) = encode l bencodeList
  encoder (BDict  d) = encode d bencodeDict

bencodeInt :: Codec Int
bencodeInt = constant "i" <:>> asciiDigits <<:> constant "e"

bencodeBytes :: Codec S.ByteString
bencodeBytes = header S.length len firstN where
  len = asciiDigits <<:> constant ":"

bencodeList :: Codec [BValue]
bencodeList = constant "l" <:>> elementwise bencode <<:> constant "e"

bencodeDict :: Codec [(S.ByteString, BValue)]
bencodeDict = constant "d" <:>> codec <<:> constant "e" where
  codec = (decoder, contramap (sortBy $ comparing fst) encoder)
  (decoder, encoder) = elementwise $ bencodeBytes <++> bencode
~~~~

Contributing
------------

Bug reports, pull requests, and comments are very welcome!

Feel free to contact me on GitHub, through IRC (#haskell on freenode),
or email (mike@barrucadu.co.uk).

[build-status]: http://ci.barrucadu.co.uk/job/(wheat)/job/wheat/badge/icon?style=plastic
[build-log]:    http://ci.barrucadu.co.uk/job/(wheat)/job/wheat/
[docs]:         https://barrucadu.github.io/wheat

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | An implementation of Bencoding, a very simple encoding format
-- commonly used in bittorrent files.
--
--There are four types of value:
--
-- - ByteStrings, encoded as <length>:<contents>
--
-- - Unbounded integers, encoded as i<base 10 ascii>e
--
-- - Lists, encoded as l<contents>e
--
-- - Dictionaries (with ByteString keys), encoded as d<contents>e,
--   where the contents are encoded as <encoded key><encoded value>,
--   with keys in lexicographical order.
--
-- Bencoding is simple and not terribly efficient, but it has its
-- uses: it is unaffected by endianness, and there is exactly one
-- legal encoding for a given data structure.
module Bencode where

-- For the implementation of Bencoding.
import Data.List (sortBy)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Profunctor (lmap)
import GHC.Generics (Generic)

import qualified Data.ByteString as S

-- For the test case. Smallcheck is used with Tasty to generate
-- progressively larger examples to try. This does mean a lot of tests
-- are generated for the recursive cases ('BList' and 'BDict'), but
-- failing cases should be minimal.
import Test.SmallCheck.Series (Serial(..))
import Test.SmallCheck.Series.Instances ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.SmallCheck (testProperty)

-- Local imports
import Data.Wheat
import Utils

-- * Tests

testBencode :: TestTree
testBencode = testGroup "Bencoding"
  [ testProperty "bencodeInt"   $ \i  -> encDec bencodeInt   i  == Just i
  , testProperty "bencodeBytes" $ \bs -> encDec bencodeBytes bs == Just bs
  , testProperty "bencodeList"  $ \vs -> encDec bencodeList  vs == Just vs
  , testProperty "bencodeDict"  $ \ds -> encDec bencodeDict  ds == Just (sortBy (comparing fst) ds)
  , testProperty "bencode" $ \b -> encDec bencode b == Just (mangleDicts b)
  ]

  where
    mangleDicts (BDict ds) = BDict $ sortBy (comparing fst) ds
    mangleDicts (BList vs) = BList $ map mangleDicts vs
    mangleDicts b = b

-- * Implementation

data BValue =
    BInt Int
  | BBytes S.ByteString
  | BList [BValue]
  | BDict [(S.ByteString, BValue)]
  deriving (Generic, Eq, Read, Show)

instance Monad m => Serial m BValue

bencode :: Codec BValue
bencode =
  wrap (Just . BInt)   (\case { BInt   i -> Just i; _ -> Nothing }) bencodeInt   <>
  wrap (Just . BBytes) (\case { BBytes b -> Just b; _ -> Nothing }) bencodeBytes <>
  wrap (Just . BList)  (\case { BList  l -> Just l; _ -> Nothing }) bencodeList  <>
  wrap (Just . BDict)  (\case { BDict  d -> Just d; _ -> Nothing }) bencodeDict

bencodeInt :: Codec Int
bencodeInt = constant "i" <:>> asciiDigits <<:> constant "e"

bencodeBytes :: Codec S.ByteString
bencodeBytes = header S.length len firstN where
  len = asciiDigits <<:> constant ":"

bencodeList :: Codec [BValue]
bencodeList = constant "l" <:>> elementwise bencode <<:> constant "e"

bencodeDict :: Codec [(S.ByteString, BValue)]
bencodeDict = constant "d" <:>> codec <<:> constant "e" where
  codec = lmap (sortBy $ comparing fst) list
  list  = elementwise $ bencodeBytes <++> bencode

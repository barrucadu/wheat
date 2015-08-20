-- | An implementation of CTCP coding
--
-- CTCP (Client To Client Protocol) is a way of sending arbitrary data
-- over an IRC network, which may include bytes not allowed in
-- standard IRC messages. CTCPs are sent as a PRIVMSG or NOTICE, where
-- the first and last characters as @\\001@ (SOH), and special bytes
-- are escaped by encoding them into a two-byte sequence beginning
-- with @\\020@ (DLE). CTCPs consist of command name (typically in
-- upper-case) followed by list of space-separated arguments, which
-- may be empty.
--
-- One use of CTCPs supported by the vast majority of IRC clients
-- today is the ACTION command, typically invoked with /me. For
-- example, if the user @foo@ in the channel @#bar@ were to issue
--
--     /me dances
--
-- everyone in the channel would receive the message
--
--     :foo PRIVMSG #bar :\001ACTION dances\001
--
-- Other common uses of CTCP include requesting the name and version
-- of a user's IRC client, their local time, determining ping times,
-- and initiating file transfers (DCC).
--
-- Characters are escaped as follows:
--
--  - `\000` (NUL) -> `\020 \060` ("0")
--
--  - `\012` (NL)  -> `\020 \156` ("n")
--
--  - `\015` (CR)  -> `\020 \162` ("r")
--
--  - `\020` (DLE) -> `\020 \020`
--
-- All other appearences of the escape character are errors, and are
-- dropped.

module CTCP where

import Data.ByteString
import Data.ByteString.Builder
import Data.Wheat
import Test.SmallCheck.Series.Instances ()
import Test.Tasty
import Test.Tasty.SmallCheck

-- * Tests

testCTCP :: TestTree
testCTCP = testGroup "CTCP"
  [ testProperty "CTCP" $ \bs -> encDec ctcp bs == Just bs ]

  where
    encDec c a = decode (toLazyByteString $ encode a c) c

-- * Implementation

ctcp :: Codec ByteString
ctcp = delimited (Just $ pack [soh]) (Just $ pack [soh]) $ codewords encodings where
  encodings =
    [(pack [p], pack [esc, c]) | (p, c) <- escapes] ++ [(pack [esc], pack [])]

soh :: Integral i => i
soh = 0o001

esc :: Integral i => i
esc = 0o020

escapes :: Integral i => [(i, i)]
escapes =
  [ (0o000, 0o060)
  , (0o012, 0o156)
  , (0o015, 0o162)
  , (0o020, 0o020)
  ]
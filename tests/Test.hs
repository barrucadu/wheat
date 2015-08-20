import Bencode (testBencode)
import CTCP    (testCTCP)

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [testBencode, testCTCP]

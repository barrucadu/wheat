import Test.Tasty (defaultMain, testGroup)
import Bencode (testBencode)

main :: IO ()
main = defaultMain $ testGroup "Tests" [testBencode]

import Test.Block
import Test.EncryptDecrypt
import Test.Key
import Test.Nibble
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "MiniAES tests" [nibbleTests, blockTests, keyTests, encryptTests]

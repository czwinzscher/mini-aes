{-# LANGUAGE BinaryLiterals #-}

import Test.Tasty

import Test.Block
import Test.EncryptDecrypt
import Test.Key
import Test.Nibble

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "MiniAES tests" [nibbleTests, blockTests, keyTests, encryptTests]

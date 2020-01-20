{-# LANGUAGE BinaryLiterals #-}

module Test.Encrypt
  ( encryptTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import MiniAES
import Test.Block ()
import Test.Nibble ()

encryptTests :: TestTree
encryptTests =
  testGroup "encrypt tests" [encryptUnitTests, encryptPropertyTests]

encryptUnitTests :: TestTree
encryptUnitTests =
  testGroup
    "encryption unit tests"
    [ testCase "encrypt test 1" $ do
        let m =
              Block
                (Nibble 0b1000)
                (Nibble 0b0111)
                (Nibble 0b1111)
                (Nibble 0b1011)
            k =
              Block
                (Nibble 0b1100)
                (Nibble 0b0011)
                (Nibble 0b1111)
                (Nibble 0b0000)
            expected =
              Block
                (Nibble 0b0110)
                (Nibble 0b1101)
                (Nibble 0b1111)
                (Nibble 0b0111)
        encrypt m k @?= expected
    , testCase "encrypt test 2" $ do
        let m =
              Block
                (Nibble 0b1000)
                (Nibble 0b0111)
                (Nibble 0b1111)
                (Nibble 0b1011)
            k =
              Block
                (Nibble 0b1001)
                (Nibble 0b0001)
                (Nibble 0b1101)
                (Nibble 0b1010)
            expected =
              Block
                (Nibble 0b1001)
                (Nibble 0b0111)
                (Nibble 0b0011)
                (Nibble 0b0110)
        encrypt m k @?= expected
    ]

encryptPropertyTests :: TestTree
encryptPropertyTests =
  testGroup
    "encrypt property tests"
    [ testProperty "encrypt and decrypt returns the old message" $ \message key ->
        decrypt (encrypt message key) key == message
    ]

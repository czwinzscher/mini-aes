{-# LANGUAGE BinaryLiterals #-}

module Test.Block
  ( blockTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import MiniAES
import Test.Instances ()

blockTests :: TestTree
blockTests = testGroup "block tests" [blockUnitTests]

blockUnitTests :: TestTree
blockUnitTests =
  testGroup
    "block unit tests"
    [ testCase "block add" $ do
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
                (Nibble 0b0100)
                (Nibble 0b0100)
                (Nibble 0b0000)
                (Nibble 0b1011)
        blockAdd m k @?= expected
    , testCase "block substitution" $ do
        let m =
              Block
                (Nibble 0b0100)
                (Nibble 0b0100)
                (Nibble 0b0000)
                (Nibble 0b1011)
            expected =
              Block
                (Nibble 0b0010)
                (Nibble 0b0010)
                (Nibble 0b1110)
                (Nibble 0b1100)
        blockSubst m @?= expected
    , testCase "block shift" $ do
        let m =
              Block
                (Nibble 0b0010)
                (Nibble 0b0010)
                (Nibble 0b1110)
                (Nibble 0b1100)
            expected =
              Block
                (Nibble 0b0010)
                (Nibble 0b1100)
                (Nibble 0b1110)
                (Nibble 0b0010)
        shiftRow m @?= expected
    , testCase "mix columns" $ do
        let m =
              Block
                (Nibble 0b0010)
                (Nibble 0b1100)
                (Nibble 0b1110)
                (Nibble 0b0010)
            expected =
              Block
                (Nibble 0b1101)
                (Nibble 0b0011)
                (Nibble 0b0101)
                (Nibble 0b1001)
        mixColumn m @?= expected
    ]

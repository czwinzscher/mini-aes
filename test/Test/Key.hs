{-# LANGUAGE BinaryLiterals #-}

module Test.Key
  ( keyTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit

import MiniAES

keyTests :: TestTree
keyTests = testGroup "key tests" [keyUnitTests]

keyUnitTests :: TestTree
keyUnitTests =
  testGroup
    "key unit tests"
    [ testCase "first key constant" $ rconsOne @?= Nibble 0b0001
    , testCase "second key constant" $ rconsTwo @?= Nibble 0b0010
    , testCase "key round 1" $ do
        let k0 =
              Block
                (Nibble 0b1100)
                (Nibble 0b0011)
                (Nibble 0b1111)
                (Nibble 0b0000)
            expected =
              Block
                (Nibble 0b0011)
                (Nibble 0b0000)
                (Nibble 0b1111)
                (Nibble 0b1111)
        nextKey k0 rconsOne @?= expected
    , testCase "key round 2" $ do
        let k1 =
              Block
                (Nibble 0b0011)
                (Nibble 0b0000)
                (Nibble 0b1111)
                (Nibble 0b1111)
            expected =
              Block
                (Nibble 0b0110)
                (Nibble 0b0110)
                (Nibble 0b1001)
                (Nibble 0b0110)
        nextKey k1 rconsTwo @?= expected
    ]

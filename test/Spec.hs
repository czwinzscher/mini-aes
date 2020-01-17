{-# LANGUAGE BinaryLiterals #-}

import Test.Tasty
import Test.Tasty.HUnit

import MiniAES

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "MiniAES tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "unit tests"
    [nibbleUnitTests, blockUnitTests, keyUnitTests, encryptUnitTests]

nibbleUnitTests :: TestTree
nibbleUnitTests = testGroup "nibble unit tests" []

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
        blockSub m @?= expected
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

keyUnitTests :: TestTree
keyUnitTests =
  testGroup
    "key unit tests"
    [ testCase "first key constant" $ do rcons 1 @?= Nibble 0b0001
    , testCase "second key constant" $ do rcons 2 @?= Nibble 0b0010
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
        nextKey k0 (rcons 1) @?= expected
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
        nextKey k1 (rcons 2) @?= expected
    ]

encryptUnitTests :: TestTree
encryptUnitTests =
  testGroup
    "encryption unit tests"
    [ testCase "encrypt test 2" $ do
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

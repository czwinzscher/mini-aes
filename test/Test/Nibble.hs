{-# LANGUAGE BinaryLiterals #-}

module Test.Nibble
  ( nibbleTests,
  )
where

import MiniAES
import Test.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

nibbleTests :: TestTree
nibbleTests = testGroup "nibble tests" [nibbleUnitTests, nibblePropertyTests]

nibbleUnitTests :: TestTree
nibbleUnitTests =
  testGroup
    "nibble unit tests"
    [ testCase "nibble add" $ do
        let n1 = Nibble 0b1000
            n2 = Nibble 0b1100
            expected = Nibble 0b0100
        nibbleAdd n1 n2 @?= expected,
      testCase "nibble mul" $ do
        let n1 = Nibble 0b1011
            n2 = Nibble 0b0111
            expected = Nibble 0b0100
        nibbleMul n1 n2 @?= expected
    ]

nibblePropertyTests :: TestTree
nibblePropertyTests =
  testGroup
    "nibble property tests"
    [ testProperty "nibbleMul returns 4 bit number" $ \n1 n2 ->
        case n1 `nibbleMul` n2 of
          (Nibble n) -> n <= 15
    ]

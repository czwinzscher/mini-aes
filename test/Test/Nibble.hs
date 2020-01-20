{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE BinaryLiterals #-}

module Test.Nibble
  ( nibbleTests
  ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.SmallCheck.Series

import MiniAES

instance Monad m => Serial m Nibble where
  series = cons1 Nibble

nibbleTests :: TestTree
nibbleTests = testGroup "nibble tests" [nibbleUnitTests]

nibbleUnitTests :: TestTree
nibbleUnitTests =
  testGroup
    "nibble unit tests"
    [ testCase "nibble add" $ do
        let n1 = Nibble 0b1000
            n2 = Nibble 0b1100
            expected = Nibble 0b0100
        nibbleAdd n1 n2 @?= expected
    , testCase "nibble mul" $ do
        let n1 = Nibble 0b1011
            n2 = Nibble 0b0111
            expected = Nibble 0b0100
        nibbleMul n1 n2 @?= expected
    ]

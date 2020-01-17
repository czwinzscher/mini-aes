{-# LANGUAGE BinaryLiterals #-}

module Main where

import MiniAES

import Numeric (showIntAtBase)

main :: IO ()
main = do
  let m = Block (Nibble 0b1000) (Nibble 0b0111) (Nibble 0b1111) (Nibble 0b1011)
      k = Block (Nibble 0b1100) (Nibble 0b0011) (Nibble 0b1111) (Nibble 0b0000)
  print $ encrypt m k

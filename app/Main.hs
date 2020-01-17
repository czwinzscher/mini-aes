module Main where

import MiniAES

import Control.Applicative ((<$>))
import Data.Char (digitToInt)
import Data.Word (Word8)
import System.Environment (getArgs)

wordFromBinString :: String -> Word8
wordFromBinString = foldl (\acc x -> acc * 2 + fromIntegral (digitToInt x)) 0

main :: IO ()
main = do
  [messageString, keyString] <- getArgs
  let [m1, m2, m3, m4] = Nibble . wordFromBinString <$> words messageString
      [k1, k2, k3, k4] = Nibble . wordFromBinString <$> words keyString
      m = Block m1 m2 m3 m4
      k = Block k1 k2 k3 k4
  print $ encrypt m k

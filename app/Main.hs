{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import MiniAES

import Control.Monad (forM, unless)
import Data.Char (digitToInt)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word8)
import System.Environment (getArgs)
import System.Exit (exitFailure)

-- The entered string should be in the form "xxxx xxxx xxxx xxxx"
isValidBinString :: T.Text -> Bool
isValidBinString str =
  let parts = T.words str
      validChar x = x == '0' || x == '1'
   in length parts == 4 &&
      foldr (\w -> (&&) (T.foldr (\c -> (&&) (validChar c)) True w)) True parts

wordFromBinString :: T.Text -> Word8
wordFromBinString = T.foldl (\acc x -> acc * 2 + fromIntegral (digitToInt x)) 0

main :: IO ()
main = do
  [command, messageString, keyString] <-
    getArgs >>= \case
      [] ->
        forM ["encrypt or decrypt?", "message:", "key:"] $ \s ->
          putStrLn s >> TIO.getLine
      a@[_, _, _] -> return (T.pack <$> a)
      _ ->
        putStrLn "usage: mini-aes [encrypt|decrypt] [message] [key]" >>
        exitFailure
  unless (isValidBinString messageString && isValidBinString keyString) $
    putStrLn "binary strings should be in the form of 'xxxx xxxx xxxx xxxx'" >>
    exitFailure
  let [m1, m2, m3, m4] = Nibble . wordFromBinString <$> T.words messageString
      [k1, k2, k3, k4] = Nibble . wordFromBinString <$> T.words keyString
      m = Block m1 m2 m3 m4
      k = Block k1 k2 k3 k4
  case command of
    "encrypt" -> print $ encrypt m k
    "decrypt" -> print $ decrypt m k
    _ -> putStrLn "command can be either encrypt or decrypt" >> exitFailure

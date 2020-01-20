{-# LANGUAGE BinaryLiterals #-}

module MiniAES.Key
  ( rcons
  , nextKey
  ) where

import MiniAES.Block
import MiniAES.Nibble

rcons :: Int -> Nibble
rcons n
  | n == 1 = Nibble 0b0001
  | n == 2 = Nibble 0b0010
  | otherwise = error "only valid values are 1 and 2"

nextKey :: Block -> Nibble -> Block
nextKey (Block b0 b1 b2 b3) rcon =
  let w0 = nibbleAdd (nibbleAdd b0 (nibbleSubst b3)) rcon
      w1 = nibbleAdd b1 w0
      w2 = nibbleAdd b2 w1
      w3 = nibbleAdd b3 w2
   in Block w0 w1 w2 w3

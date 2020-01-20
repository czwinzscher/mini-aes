{-# LANGUAGE BinaryLiterals #-}

module MiniAES.Block
  ( Block(..)
  , blockAdd
  , blockSub
  , encrypt
  -- , decrypt
  , shiftRow
  , mixColumn
  , rcons
  , nextKey
  ) where

import MiniAES.Nibble

data Block =
  Block Nibble Nibble Nibble Nibble
  deriving (Eq, Ord)

instance Show Block where
  show (Block b0 b1 b2 b3) = unwords [show b0, show b1, show b2, show b3]

blockAdd :: Block -> Block -> Block
blockAdd (Block n11 n12 n13 n14) (Block n21 n22 n23 n24) =
  Block
    (nibbleAdd n11 n21)
    (nibbleAdd n12 n22)
    (nibbleAdd n13 n23)
    (nibbleAdd n14 n24)

blockSub :: Block -> Block
blockSub (Block b0 b1 b2 b3) =
  Block (nibbleSubst b0) (nibbleSubst b1) (nibbleSubst b2) (nibbleSubst b3)

shiftRow, mixColumn :: Block -> Block
shiftRow (Block b0 b1 b2 b3) = Block b0 b3 b2 b1

mixColumn (Block c0 c1 c2 c3) =
  let calcCol cFirst cSecond =
        ( nibbleMul (Nibble 0b0011) cFirst `nibbleAdd`
          nibbleMul (Nibble 0b0010) cSecond
        , nibbleMul (Nibble 0b0010) cFirst `nibbleAdd`
          nibbleMul (Nibble 0b0011) cSecond)
      (d0, d1) = calcCol c0 c1
      (d2, d3) = calcCol c2 c3
   in Block d0 d1 d2 d3

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

encrypt :: Block -> Block -> Block
encrypt message key = encryptRec message key 0

encryptRec :: Block -> Block -> Int -> Block
encryptRec message key roundNum
  | roundNum == 2 = message `blockAdd` key
  | roundNum > 2 || roundNum < 0 = message
  | otherwise =
    let newMessage =
          ((if roundNum == 0
              then mixColumn
              else id) .
           shiftRow . blockSub . blockAdd key)
            message
        keyNext = nextKey key (rcons (roundNum + 1))
     in encryptRec newMessage keyNext (roundNum + 1)
-- decrypt :: Block -> Block -> Block
-- decrypt message key = message

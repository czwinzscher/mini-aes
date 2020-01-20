{-# LANGUAGE BinaryLiterals #-}

module MiniAES.Block
  ( Block(..)
  , blockAdd
  , blockSub
  , blockSubReversed
  , shiftRow
  , mixColumn
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

blockSubReversed :: Block -> Block
blockSubReversed (Block b0 b1 b2 b3) =
  Block
    (nibbleSubstReversed b0)
    (nibbleSubstReversed b1)
    (nibbleSubstReversed b2)
    (nibbleSubstReversed b3)

shiftRow, mixColumn :: Block -> Block
shiftRow (Block b0 b1 b2 b3) = Block b0 b3 b2 b1

mixColumn (Block c0 c1 c2 c3) =
  let calcCol cFirst cSecond =
        ( (Nibble 0b0011 `nibbleMul` cFirst) `nibbleAdd`
          (Nibble 0b0010 `nibbleMul` cSecond)
        , (Nibble 0b0010 `nibbleMul` cFirst) `nibbleAdd`
          (Nibble 0b0011 `nibbleMul` cSecond))
      (d0, d1) = calcCol c0 c1
      (d2, d3) = calcCol c2 c3
   in Block d0 d1 d2 d3

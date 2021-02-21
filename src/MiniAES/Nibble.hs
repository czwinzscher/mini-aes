{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE OverloadedLists #-}

module MiniAES.Nibble
  ( Nibble (..),
    substTable,
    substTableReversed,
    nibbleSubst,
    nibbleSubstReversed,
    nibbleAdd,
    nibbleMul,
  )
where

import Data.Bits (shiftL, testBit, xor)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Word (Word8)
import Text.Printf

newtype Nibble
  = Nibble Word8
  deriving (Eq, Ord)

instance Show Nibble where
  show (Nibble x) = printf "%04b" x

substTable :: Map.Map Nibble Nibble
substTable =
  [ (Nibble 0b0000, Nibble 0b1110),
    (Nibble 0b0001, Nibble 0b0100),
    (Nibble 0b0010, Nibble 0b1101),
    (Nibble 0b0011, Nibble 0b0001),
    (Nibble 0b0100, Nibble 0b0010),
    (Nibble 0b0101, Nibble 0b1111),
    (Nibble 0b0110, Nibble 0b1011),
    (Nibble 0b0111, Nibble 0b1000),
    (Nibble 0b1000, Nibble 0b0011),
    (Nibble 0b1001, Nibble 0b1010),
    (Nibble 0b1010, Nibble 0b0110),
    (Nibble 0b1011, Nibble 0b1100),
    (Nibble 0b1100, Nibble 0b0101),
    (Nibble 0b1101, Nibble 0b1001),
    (Nibble 0b1110, Nibble 0b0000),
    (Nibble 0b1111, Nibble 0b0111)
  ]

substTableReversed :: Map.Map Nibble Nibble
substTableReversed =
  [ (Nibble 0b0000, Nibble 0b1110),
    (Nibble 0b0001, Nibble 0b0011),
    (Nibble 0b0010, Nibble 0b0100),
    (Nibble 0b0011, Nibble 0b1000),
    (Nibble 0b0100, Nibble 0b0001),
    (Nibble 0b0101, Nibble 0b1100),
    (Nibble 0b0110, Nibble 0b1010),
    (Nibble 0b0111, Nibble 0b1111),
    (Nibble 0b1000, Nibble 0b0111),
    (Nibble 0b1001, Nibble 0b1101),
    (Nibble 0b1010, Nibble 0b1001),
    (Nibble 0b1011, Nibble 0b0110),
    (Nibble 0b1100, Nibble 0b1011),
    (Nibble 0b1101, Nibble 0b0010),
    (Nibble 0b1110, Nibble 0b0000),
    (Nibble 0b1111, Nibble 0b0101)
  ]

nibbleSubst :: Nibble -> Nibble
nibbleSubst n = fromJust $ Map.lookup n substTable

nibbleSubstReversed :: Nibble -> Nibble
nibbleSubstReversed n = fromJust $ Map.lookup n substTableReversed

nibbleAdd :: Nibble -> Nibble -> Nibble
nibbleAdd (Nibble x1) (Nibble x2) = Nibble (x1 `xor` x2)

nibbleMul :: Nibble -> Nibble -> Nibble
nibbleMul (Nibble x1) (Nibble x2) =
  let mulRes =
        foldr
          ( \(b, i) r ->
              if b
                then shiftL x1 i `xor` r
                else r
          )
          0
          [(testBit x2 i, i) | i <- [0 .. 3]]
      c = 0b10011
      nibbleMod n =
        foldr
          ( \(b, i) r ->
              if b
                then r `xor` shiftL c (i - 4)
                else r
          )
          n
          [(testBit n i, i) | i <- [4 .. 6]]
   in Nibble (nibbleMod mulRes)

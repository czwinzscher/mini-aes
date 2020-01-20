module MiniAES.Decrypt
  ( decrypt
  ) where

import MiniAES.Block
import MiniAES.Key

decrypt :: Block -> Block -> Block
decrypt message k0 =
  let k1 = nextKey k0 (rcons 1)
      k2 = nextKey k1 (rcons 2)
   in (blockAdd k0 .
       shiftRow .
       blockSubReversed .
       mixColumn . blockAdd k1 . shiftRow . blockSubReversed . blockAdd k2)
        message

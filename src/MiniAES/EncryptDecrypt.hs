module MiniAES.EncryptDecrypt
  ( encrypt
  , decrypt
  ) where

import MiniAES.Block
import MiniAES.Key

encrypt :: Block -> Block -> Block
encrypt message k0 =
  let k1 = nextKey k0 rconsOne
      k2 = nextKey k1 rconsTwo
   in (blockAdd k2 .
       shiftRow .
       blockSubst .
       blockAdd k1 . mixColumn . shiftRow . blockSubst . blockAdd k0)
        message

decrypt :: Block -> Block -> Block
decrypt message k0 =
  let k1 = nextKey k0 rconsOne
      k2 = nextKey k1 rconsTwo
   in (blockAdd k0 .
       shiftRow .
       blockSubstReversed .
       mixColumn . blockAdd k1 . shiftRow . blockSubstReversed . blockAdd k2)
        message

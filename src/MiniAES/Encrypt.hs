module MiniAES.Encrypt
  ( encrypt
  ) where

import MiniAES.Block
import MiniAES.Key

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

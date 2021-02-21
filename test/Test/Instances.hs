{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Instances where

import MiniAES (Block (..), Nibble (..))
import Test.SmallCheck.Series

instance Monad m => Serial m Nibble where
  series = cons1 Nibble

instance Monad m => Serial m Block where
  series = cons4 Block

{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators #-}

module MODULE_NAME where

import Data.Array.Accelerate                  as Acc
import Data.Array.Accelerate.Array.BlockCopy  as ABC

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The user's acutal effect
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

job :: Array DIM3 Word8 -> Acc (Array DIM3 Word8)
job arr = Acc.map (intToWord8) $ effect $ Acc.map (word8ToInt) $ use arr
  where
    -- Useful values to bring into scope
    arrDim       = arrayShape arr
    (Z:.h:.w:.d) = arrDim

    effect :: Acc (Array DIM3 Int) -> Acc (Array DIM3 Int)
{-# LINE 1 "Code.hs" #-}

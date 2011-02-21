{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators #-}

module MODULE_NAME where

import Data.Array.Accelerate                  as Acc
import Data.Array.Accelerate.Array.BlockCopy  as ABC

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Useful functions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- | Convert RGBA image to grayscale.
--
toGray :: Acc (Array DIM3 Int) -> Acc (Array DIM2 Int)
toGray = Acc.fold (+) (constant 0) . Acc.map (`div` 3) . rgbOnly
  where
    -- remove the alpha channel
    rgbOnly :: Acc (Array DIM3 Int) -> Acc (Array DIM3 Int)
    rgbOnly arr = Acc.backpermute (lift (Z :. h :. w :. (3::Int))) id arr
      where
        (Z :. h :. w :. _) = unlift $ shape arr :: (Z :. Exp Int :. Exp Int :. Exp Int)


-- | Convert grayscale image to RGBA.
--
toRgb :: Acc (Array DIM2 Int) -> Acc (Array DIM3 Int)
toRgb arr = Acc.generate (lift (Z :. h :. w :. (4::Int))) avg
  where
    (Z :. h :. w) = unlift $ shape arr :: (Z :. Exp Int :. Exp Int)

    avg ix = (d ==* 3) ? (255, arr ! (lift (Z :. y :. x)))
      where
        (Z :. y :. x :. d) = unlift ix :: (Z :. Exp Int :. Exp Int :. Exp Int)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The user's actual effect
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
JOB :: Array DIM3 Word8 -> Acc (Array DIM3 Word8)
JOB arr = Acc.map (intToWord8) $ effect $ Acc.map (word8ToInt) $ use arr
  where
    -- Useful values to bring into scope
    arrDim       = arrayShape arr
    (Z:.h:.w:.d) = arrDim

    effect :: Acc (Array DIM3 Int) -> Acc (Array DIM3 Int)
{-# LINE 1 "Code.hs" #-}

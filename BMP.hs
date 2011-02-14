{-# LANGUAGE CPP, ScopedTypeVariables, TypeOperators #-}

module BMP where

import Data.Array.Accelerate as Acc
import Data.Array.Accelerate.Array.BlockCopy
import Control.Monad
import Codec.BMP
import Data.ByteString

import System.IO
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Image I/O
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Convert bitmap value to 3D accelerate array.
--
bmpToArray :: FilePath -> IO (Array DIM3 Word8)
bmpToArray bmpFile = do
  bmp  <- liftM (either (error . show) id) $ readBMP bmpFile
  let (w, h) = bmpDimensions bmp
      dim    = (Z :. h :. w :. 4)
  byteStringsToArray dim ((), unpackBMPToRGBA32 bmp)

-- | Convert 3D accelerate array to a bitmap value.
--
arrayToBmp :: FilePath -> Array DIM3 Word8 -> IO ()
arrayToBmp bmpFile arr = do
  ((), bs) <- arrayToByteStrings arr
  let bmp = packRGBA32ToBMP w h bs
  h <- openFile bmpFile WriteMode
  hPutBMP h bmp
  hClose h
  where
    (Z :. h :. w :. _) = arrayShape arr


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Useful functions
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- | Convert RGBA image to grayscale.
--
toGray :: Acc (Array DIM3 Int) -> Acc (Array DIM2 Int)
toGray = Acc.fold (+) (constant 0) . Acc.map (`div` 3)


-- | Convert grayscale image to RGBA.
--
toRgb :: Acc (Array DIM2 Int) -> Acc (Array DIM3 Int)
toRgb = Acc.replicate (constant (Z :. All :. All :. (4::Int)))

runEffectJob :: (Array DIM3 Word8 -> Array DIM3 Word8) -> (String, String) -> IO ()
runEffectJob job (imageInBmp, imageOutBmp) = do
  arrIn  <- bmpToArray imageInBmp
  let arrOut = job arrIn
  arrayToBmp imageOutBmp arrOut

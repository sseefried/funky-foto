{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module Main where

import Data.Array.Accelerate                  as Acc
import Data.Array.Accelerate.Array.BlockCopy  as ABC
import Data.Array.Accelerate.CUDA             as CUDA

import Codec.BMP

import System
import System.IO

import Control.Monad


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The effect program: ./prog in-jpg-file out-jpg-file
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
main = do
  [inFile, outFile] <- getArgs

  arrIn  <- bmpToArray inFile
  let arrOut = job arrIn
  arrayToBmp outFile arrOut


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


-- | Convert 3D acclerate array to a bitmap value.
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


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The user's acutal effect
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

job :: Array DIM3 Word8 -> Array DIM3 Word8
job arr = CUDA.run $ Acc.map (intToWord8) $ effect $ Acc.map (word8ToInt) $ use arr
  where
    -- Useful values to bring into scope
    arrDim       = arrayShape arr
    (Z:.h:.w:.d) = arrDim

    effect :: Acc (Array DIM3 Int) -> Acc (Array DIM3 Int)

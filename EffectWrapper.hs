{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}

module Main where

import Data.Array.Accelerate                  as Acc
import Data.Array.Accelerate.Array.BlockCopy  as ABC
import Data.Array.Accelerate.CUDA             as CUDA

import Codec.BMP

import System
import System.IO

import Control.Monad


-- | The effect program: ./prog in-bitmap-file out-bitmap-file
--
main = do
  [inFile, outFile] <- getArgs

  bmpIn  <- liftM (either (error . show) id) $ readBMP inFile
  arrIn  <- bmpToArray bmpIn

  bmpOut <- arrayToBmp (job arrIn)
  h      <- openFile outFile WriteMode
  hPutBMP h bmpOut
  hClose h


-- | Convert bitmap value to 3D accelerate array.
--
bmpToArray :: BMP -> IO (Array DIM3 Word8)
bmpToArray bmp = byteStringsToArray dim ((), unpackBMPToRGBA32 bmp)
  where
    (w, h) = bmpDimensions bmp
    dim    = (Z :. h :. w :. 4)

-- | Convert 3D acclerate array to a bitmap value.
--
arrayToBmp :: Array DIM3 Word8 -> IO BMP
arrayToBmp arr = do
  ((),bs) <- arrayToByteStrings arr
  return $ packRGBA32ToBMP w h bs
  where
    (Z :. h :. w :. _) = arrayShape arr



-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The user's acutal effect
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

job :: Array DIM3 Word8 -> Array DIM3 Word8
job arr = CUDA.run $ effect $ use arr

effect :: Acc (Array DIM3 Word8) -> Acc (Array DIM3 Word8)


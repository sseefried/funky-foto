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
-- The user's acutal effect
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

job :: Array DIM3 Word8 -> Array DIM3 Word8
job arr = CUDA.run $ effect $ use arr

effect :: Acc (Array DIM3 Word8) -> Acc (Array DIM3 Word8)

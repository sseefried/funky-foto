{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Root where

import Foundation

import Data.Array.Accelerate      as Acc
import Data.Array.Accelerate.Array.BlockCopy as ABC
import Data.Array.Accelerate.CUDA as CUDA
import Codec.BMP

import Data.ByteString            as BS
import Data.ByteString.Lazy       as BSL

import Control.Monad
import IO

-- Following four imports needed for bmpToArray and arrayToBmp
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array

-- |'GET' homepage.
--
getHomeR :: Handler RepHtml
getHomeR =
    defaultLayout $ do
      setTitle "Recogniser!"
      addWidget $(widgetFile "homepage")


-- |'POST' to run page for processing an image.
--
postRunR :: Handler RepHtml
postRunR = do
  rr <- getRequest
  (_, files) <- liftIO $ reqRequestBody rr
  fi <- maybe notFound return $ lookup "file" files

  let imageInName  = "in-"  ++ fileName fi
      imageOutName = "out-" ++ fileName fi
      imageIn  = ImageR imageInName
      imageOut = ImageR imageOutName

  -- save the input file
  liftIO $ BSL.writeFile imageInName $ fileContent fi

  -- process the file + save the ouput file
  bmpIn <- liftIO $ liftM (either (error . show) id) $ readBMP imageInName    -- FIX: Could return an error

  arrIn <- liftIO $ bmpToArray bmpIn
  bmpOut <- liftIO $ arrayToBmp (processImage arrIn)

--  liftIO $ writeBMP imageOutName bmpOut

 -- use this code if writeBMP hasn't been patched in Codec.BMP
  h <- liftIO $ openFile imageOutName WriteMode
  liftIO $ hPutBMP h bmpOut
  liftIO $ hClose h

  -- render both input and output images
  defaultLayout $ do
    setTitle $ string $ fileName fi
    addWidget $(widgetFile "run")


-- |'GET' an image. Referenced by 'img' HTML tags.
--
getImageR :: String -> Handler ()
getImageR name = sendFile "image/bmp" name


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- "Backend" processing code.


-- |Use Acceleate CUDA backend to 'flip (spin)' the image.
--
processImage :: Array DIM3 Word8 -> Array DIM3 Word8
processImage arr = CUDA.run job
  where
    job = Acc.backpermute dim spin (use arr)
    dim = constant d

    d@(Z :. _ :. w :. _) = arrayShape arr

    spin ix = let (Z :. y :. x :. c) :: (Z :. Exp Int :. Exp Int :. Exp Int) = Acc.unlift ix
              in
                Acc.lift (Z :. y :. (constant w) - x - 1:. c)



-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Move the following features into a separate package for image I/O


-- |Convert bitmap value to 3D accelerate array.
--
bmpToArray :: BMP -> IO (Array DIM3 Word8)
bmpToArray bmp = byteStringsToArray dim ((), unpackBMPToRGBA32 bmp)
  where
    (w, h) = bmpDimensions bmp
    dim    = (Z :. h :. w :. 4)

arrayToBmp :: Array DIM3 Word8 -> IO BMP
arrayToBmp arr = do
  ((),bs) <- arrayToByteStrings arr
  return $ packRGBA32ToBMP w h bs
  where
    (Z :. h :. w :. _) = arrayShape arr


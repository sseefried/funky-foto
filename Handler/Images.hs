{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Images where

-- standard libraries
import Data.ByteString.Lazy       as BL
import System.Directory
import System.FilePath
import System.Cmd

-- friends
import Foundation


-- | Retrieve image resources.
--   FIXME:
--      - support different image formats (e.g. png)
--
getImageR :: String -> Handler ()
getImageR imgName = do
  foundation <- getYesod
  sendFile "image/jpg" $ imageFile (cacheDir foundation) imgName


-- | For a given image "name", return its location on disk.
--
imageFile :: FilePath -> String -> FilePath
imageFile cacheD imgName = cacheD </> "images" </> imgName


-- | Convert a JPEG file to a bitmap file.
--
jpgToBmp :: FilePath -> FilePath -> IO ()
jpgToBmp jpgFile bmpFile = rawSystem "convert" [jpgFile, bmpFile] >> return ()


-- | Convert a bitmap file to a JPEG file.
--
bmpToJpg :: FilePath -> FilePath -> IO ()
bmpToJpg bmpFile jpgFile = rawSystem "convert" [bmpFile, jpgFile] >> return ()


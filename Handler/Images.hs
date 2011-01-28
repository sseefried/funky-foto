{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Images where

-- standard libraries
import Data.ByteString.Lazy       as BL
import System.Directory
import System.FilePath

-- friends
import Foundation


-- | Retrieve image resources.
--   FIXME:
--      - support different image formats (e.g. png)
--
getImageR :: String -> Handler ()
getImageR imgName = do
  foundation <- getYesod
  sendFile "image/bmp" $ imageFile (cacheDir foundation) imgName


-- | For a given image "name", return its location on disk.
--
imageFile :: FilePath -> String -> FilePath
imageFile cacheD imgName = cacheD </> "images" </> imgName


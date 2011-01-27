{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Images where

-- standard libraries
import Data.ByteString.Lazy       as BL

-- friends
import Foundation


-- | Retrieve image resources.
--   FIXME:
--      - support different image formats (e.g. png)
--
getImageR :: String -> Handler ()
getImageR name = sendFile "image/bmp" $ "tmp/" ++ name


-- | For a given image "name", return its location on disk.
--
imageFile :: String -> FilePath
imageFile = (++) "tmp/"


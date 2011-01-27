{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Root where

-- friends
import Foundation


-- | Show homepage.
--
getHomeR :: Handler RepHtml
getHomeR =
    defaultLayout $ do
      setTitle "Funky Foto!"
      addWidget $(widgetFile "homepage")


-- | Retrieve image resources.
--   FIXME:
--      - store images in a separate database table.
--      - support different image formats (e.g. png)
--
getImageR :: String -> Handler ()
getImageR name = sendFile "image/bmp" name


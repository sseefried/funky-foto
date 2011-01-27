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


{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Root where

-- friends
import Foundation


-- | Root page redirects immediately to effects list page.
--
getHomeR :: Handler RepHtml
getHomeR = redirect RedirectSeeOther ListEffectsR


-- | 'About' page.
--
getAboutR :: Handler RepHtml
getAboutR = defaultLayout $ addWidget $(widgetFile "about")

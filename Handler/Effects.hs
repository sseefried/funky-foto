{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Effects where

-- standard libraries
import Control.Concurrent.MVar
import Control.Monad
import System.IO
import System.Cmd

-- friends
import Foundation

-- Lists all the effects
getEffectsR :: Handler RepHtml
getEffectsR  = undefined

-- Show the effect (with source code, initially empty)
getEffectR :: String -> Handler RepHtml
getEffectR = undefined

-- Edit the effect
getEditEffectR :: String -> Handler RepHtml
getEditEffectR = undefined

-- Modifies an effect and returns the show page afterwards.
putEffectR :: String -> Handler RepHtml
putEffectR = undefined

-- Deletes the effect and then shows the list of effects.
deleteEffectR :: String -> Handler RepHtml
deleteEffectR = undefined

-- Creates the (empty) effect
postEffectR :: String -> Handler RepHtml
postEffectR = undefined

-- Shows the page for uploading files to run the effect
getRunEffectR :: String -> Handler RepHtml
getRunEffectR = undefined

-- Submits and runs the effect with the image and shows the result.
postRunEffectR :: String -> Handler RepHtml
postRunEffectR _ = do
    s <- getYesod
    liftIO $ withMVar (theVar s) $ \() -> do
      System.IO.putStrLn "Taking the MVar"
      system ("date +%S")
      system "sleep 5"
    defaultLayout $ do
      setTitle $ string "Waiting..."
      addWidget $(widgetFile "runEffect")


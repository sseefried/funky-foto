{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Effects where

-- standard libraries
import Control.Concurrent.MVar
import Control.Monad
import System.IO
import System.Cmd

-- friends
import Foundation
import Model

-- Lists all the effects
getEffectsR :: Handler RepHtml
getEffectsR  = undefined

-- Show the effect (with source code, initially empty)
getEffectR :: String -> Handler RepHtml
getEffectR name = do
  -- get the effect from the database.
  mbResult <- runDB $ do { getBy $ UniqueEffect name }
  defaultLayout $ do
    case mbResult of
      Just (_,effect) -> addWidget $(widgetFile "effects/show")
      Nothing         -> addWidget $(widgetFile "effects/not-found")

-- Edit the effect
getEditEffectR :: String -> Handler RepHtml
getEditEffectR = undefined

-- Creates or modifies an effect and returns the show page afterwards.
putEffectR :: String -> Handler RepHtmlJson
putEffectR name = do
  mbCode <- lookupPostParam "code"
  let code = case mbCode of { Just c  -> c; Nothing -> "no code" }
  mbEffectAndKey <- runDB $ do { getBy (UniqueEffect name) }
  (effectKey, effect) <- case mbEffectAndKey of
    Just (key,effect) -> runDB $ do { update key [EffectCode code]
                                    ; return (key, effect { effectCode = code }) }
    Nothing -> do
      effectKey   <- runDB $ do { insert (Effect name "insert code here") }
      (Just effect) <- runDB $ get effectKey
      return (effectKey, effect)
  defaultLayoutJson (do { addWidget $(widgetFile "effects/show") })
                         (jsonMap [("name", jsonScalar name)])

-- Deletes the effect and then shows the list of effects.
deleteEffectR :: String -> Handler RepHtml
deleteEffectR = undefined

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


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
getListEffectsR :: Handler RepHtml
getListEffectsR  = undefined

-- Show the effect (with source code, initially empty)
getShowEffectR :: String -> Handler RepHtml
getShowEffectR name = do
  -- get the effect from the database.
  mbResult <- runDB $ do { getBy $ UniqueEffect name }
  case mbResult of
    Just (_,effect) -> showEffect effect
    Nothing         -> defaultLayout $ addWidget $(widgetFile "effects/not-found")

showEffect :: Effect -> Handler RepHtml
showEffect effect = defaultLayout $ addWidget $(widgetFile "effects/show")

-- Edit the effect
getEditEffectR :: String -> Handler RepHtml
getEditEffectR = undefined


putCreateEffectR :: String -> Handler RepHtmlJson
putCreateEffectR = createOrUpdateEffect

postCreateEffectR :: String -> Handler RepHtmlJson
postCreateEffectR = createOrUpdateEffect

putUpdateEffectR :: String -> Handler RepHtmlJson
putUpdateEffectR = createOrUpdateEffect

postUpdateEffectR :: String -> Handler RepHtmlJson
postUpdateEffectR = createOrUpdateEffect

-- Creates or updates an effect and returns the show page afterwards.
createOrUpdateEffect :: String -> Handler RepHtmlJson
createOrUpdateEffect name = do
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
  (RepHtml html) <- showEffect effect
  json <- jsonToContent $ jsonMap [("status", jsonScalar "success")]
  return $ RepHtmlJson html json

-- Deletes the effect and then shows the list of effects.
deleteDeleteEffectR :: String -> Handler RepHtml
deleteDeleteEffectR = deleteEffect

postDeleteEffectR :: String -> Handler RepHtml
postDeleteEffectR = deleteEffect

deleteEffect :: String -> Handler RepHtml
deleteEffect = undefined

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


{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Effects where

-- standard libraries
import Control.Applicative
import Text.Hamlet
import Text.Printf

-- friends
import Settings
import Foundation
import Model
import Handler.Images


defaultEffectCode :: String
defaultEffectCode = "effect = id"

-- Lists all the effects
getListEffectsR :: Handler RepHtmlJson
getListEffectsR  = do
  -- TODO: For now just return all effects. Pagination to come.
  compilesParam <- lookupGetParam "compiles"
  let effectFilter = maybe [] (\val -> if val == "yes" then [EffectCompilesEq True] else []) compilesParam
  results <- runDB $ selectList effectFilter [EffectNameAsc] 1000 0
  let effects = map snd results
  (_, form, encType, csrfHtml) <- runFormPost $ createFormlet Nothing
  let newForm = $(widgetFile "effects/new")
      canCancel = False
      info = information ""
  let json = jsonList (map (jsonScalar . effectName) effects)
  defaultLayoutJson (addWidget $(widgetFile "effects/list")) json


-- Show the effect (with source code, initially empty)
getShowEffectR :: String -> Handler RepHtml
getShowEffectR name = do
  -- get the effect from the database.
  mbResult <- runDB $ do { getBy $ UniqueEffect name }
  case mbResult of
    Just (_,effect) -> showEffect effect
    Nothing         -> effectNotFound name

showEffect :: Effect -> Handler RepHtml
showEffect effect = defaultLayout $ do
  addWidget $(widgetFile "effects/show")
  addWidget $(widgetFile "effects/preview")

effectNotFound :: String -> Handler RepHtml
effectNotFound name = defaultLayout $ addWidget $(widgetFile "effects/not-found")

-- Edit the effect for the first time
getEditEffectR :: String -> Handler RepHtml
getEditEffectR name = do
  mbResult <- runDB (getBy (UniqueEffect name))
  case mbResult of
    Just (key, effect) -> do
      -- this is the first time we show the form so we don't care about the result type.
      (_, form, encType, csrfHtml) <- runFormPost $ editFormlet effect
      defaultLayout $ do
        addWidget $(widgetFile "effects/edit")
        addWidget $(widgetFile "effects/preview")
    Nothing            -> effectNotFound name

-- A very simple form. The only field you can edit is the code field.
-- TODO: There appears to be no way in Yesod, so far, to set attributes on form fields.
-- e.g. I want to size the textarea in the form below but can't yet.
data EditParams = EditParams { editParamsName :: String
                             , editParamsCode :: Textarea }

editFormlet :: Effect -> Form s m EditParams
editFormlet effect = do
  -- The unTextArea turns the Textarea back into a String because Effect constructor requires
  -- second argument with that type.
  fieldsToDivs $ EditParams (effectName effect) <$> textareaField "Code" (Just $ Textarea $ effectCode effect)

-- A simple form for creating a new effect.
createFormlet :: Maybe String -> Form s m String
createFormlet s = fieldsToDivs $ stringField "Add new effect" s


putCreateEffectR :: Handler RepHtml
putCreateEffectR = createEffect

postCreateEffectR :: Handler RepHtml
postCreateEffectR = createEffect

-- Creates or updates an effect and returns the show page afterwards.
createEffect :: Handler RepHtml
createEffect = do
  (res, form, encType, csrfHtml) <- runFormPost $ createFormlet Nothing
  result <- case res of
    FormMissing   -> return (Left "Name is blank" :: Either String String)
    FormFailure _ -> return (Left "There were some problems with the form")
    FormSuccess name -> return (Right name)
  let canCancel = True
  case result of
    Left infoStr -> do
      let info = information infoStr
      defaultLayout $ addWidget $(widgetFile "effects/new")
    Right name -> do
      mbGet <- runDB $ getBy (UniqueEffect name)
      case mbGet of
        Nothing -> do
          -- FIXME: Will only succeed if someone else hasn't inserted a record in the mean time.
          -- Very unlikely but still possible.
          effectKey <- runDB $ insert (Effect name defaultEffectCode True)
          -- FIXME: Very small change it's been deleted in mean time
          (Just effect) <- runDB $ get effectKey
          defaultLayout $ do
            addWidget $(widgetFile "effects/show")
            addWidget $(widgetFile "effects/preview")
        Just _ -> do
          let info = information $ printf "An effect with name '%s' already exists!" name
          defaultLayout $ addWidget $(widgetFile "effects/new")




putUpdateEffectR :: String -> Handler RepHtml
putUpdateEffectR = updateEffect

postUpdateEffectR :: String -> Handler RepHtml
postUpdateEffectR = updateEffect

updateEffect :: String -> Handler RepHtml
updateEffect name = do
  mbResult <- runDB $ getBy (UniqueEffect name)
  case mbResult of
     Just (key, effect') -> do
       (res, form, encType, csrfHtml) <- runFormPost $ editFormlet effect'
       eResult <- case res of
         FormMissing   -> return (Left "Form is blank" :: Either String EditParams)
         FormFailure _ -> return (Left $ "There were some problems with the form")
         FormSuccess params -> return (Right params)
       case eResult of
         Left info -> do
           defaultLayout $ do
             let effect = effect'
             addWidget $(widgetFile "effects/edit")
             addHtml $ information info
         Right params -> do
           let effect = Effect (editParamsName params) (unTextarea $ editParamsCode params) True
           runDB $ replace key effect
           compileRes <- compileEffect effect
           case compileRes of
             (Left err) -> do
               runDB $ replace key (effect {effectCompiles = False})
               defaultLayout $ do
                 addWidget $(widgetFile "effects/edit")
                 addHtml $ information err
                 addWidget $(widgetFile "effects/preview")
             (Right _) -> do
               runDB $ replace key (effect {effectCompiles = True})
               defaultLayout $ do
                 addWidget $(widgetFile "effects/edit")
                 addWidget $(widgetFile "effects/preview")

     Nothing -> error "die die die"-- FIXME: Need to handle this gracefully.


information :: String -> Html
information infoStr =
  if length infoStr > 0
  then $(Foundation.hamletFile "info")-- [$hamlet|div.info $str$ |]
  else ""


-- Deletes the effect and then shows the list of effects.
deleteDeleteEffectR :: String -> Handler RepHtml
deleteDeleteEffectR = deleteEffect

postDeleteEffectR :: String -> Handler RepHtml
postDeleteEffectR = deleteEffect

deleteEffect :: String -> Handler RepHtml
deleteEffect name = do
  runDB $ deleteBy $ UniqueEffect name
  redirect RedirectSeeOther ListEffectsR


-- Shows the page for uploading files to run the effect
getRunEffectR :: String -> Handler RepHtml
getRunEffectR name =
    defaultLayout $ do
      setTitle $ string $ "Run '" ++ name ++ "'"
      addWidget $(widgetFile "effects/run")


-- Submits and runs the effect with the image and shows the result.
postResultEffectR :: String -> Handler RepHtmlJson
postResultEffectR name = do
  mbResult <- runDB $ do { getBy $ UniqueEffect name }
  case mbResult of
    Just (_,effect) -> showResults effect
    Nothing         -> do
      let json = jsonMap [("image-hash", jsonScalar "error")]
      defaultLayoutJson (addWidget $(widgetFile "effects/not-found")) json
  where
    showResults :: Effect -> Handler RepHtmlJson
    showResults effect = do
      -- Get the uploaded file and save it to disk
      rr <- getRequest
      (_, files) <- liftIO $ reqRequestBody rr
      fi <- maybe notFound return $ lookup "file" files
      _  <- saveInputImage fi
      -- Render both input and result images.
      let imageInHash = imageHash fi
      let json = jsonMap [("image-hash", jsonScalar imageInHash) ]
      flip defaultLayoutJson json $ do
        setTitle $ string $ fileName fi
        addWidget $(widgetFile "effects/result")


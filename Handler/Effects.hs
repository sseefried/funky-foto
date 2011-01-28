{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Effects where

-- standard libraries
import Control.Applicative
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C

import Control.Concurrent.MVar
import System.IO
import System.Cmd
import Data.List(intersperse)
import Text.Hamlet
import Text.Printf
import System.FilePath
import System.Directory
import System.Posix.Types
import System.Posix.IO
import System.Posix.Process

import Yesod.Helpers.Static

-- friends
import Foundation
import Model
import Handler.Images


defaultEffectCode :: String
defaultEffectCode = "effect = id"

-- Lists all the effects
getListEffectsR :: Handler RepHtml
getListEffectsR  = do
  -- TODO: For now just return all effects. Pagination to come.
  results <- runDB $ selectList [] [] 1000 0
  let effects = map snd results
  (_, form, encType, csrfHtml) <- runFormPost $ createFormlet Nothing
  let newForm = $(widgetFile "effects/new")
      canCancel = False
      info = information ""
  defaultLayout $ addWidget $(widgetFile "effects/list")


-- Show the effect (with source code, initially empty)
getShowEffectR :: String -> Handler RepHtml
getShowEffectR name = do
  -- get the effect from the database.
  mbResult <- runDB $ do { getBy $ UniqueEffect name }
  case mbResult of
    Just (_,effect) -> showEffect effect
    Nothing         -> effectNotFound name

showEffect :: Effect -> Handler RepHtml
showEffect effect = defaultLayout $ addWidget $(widgetFile "effects/show")

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
      let info = ("" :: String)
      defaultLayout $ addWidget $(widgetFile "effects/edit")
    Nothing            -> effectNotFound name

-- A very simple form. The only field you can edit is the code field.
-- TODO: There appears to be no way in Yesod, so far, to set attributes on form fields.
-- e.g. I want to size the textarea in the form below but can't yet.
editFormlet :: Effect -> Form s m Effect
editFormlet effect = do
  -- The unTextArea turns the Textarea back into a String because Effect constructor requires
  -- second argument with that type.
  fieldsToDivs $ Effect (effectName effect) <$>
                  (unTextarea <$> textareaField "Code" (Just $ Textarea $ effectCode effect))

-- A simple form for creating a new effect.
createFormlet :: Maybe String -> Form s m String
createFormlet s = fieldsToDivs $ stringField "New effect" s


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
    FormSuccess effectName -> return (Right effectName)
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
          effectKey <- runDB $ insert (Effect name defaultEffectCode)
          -- FIXME: Very small change it's been deleted in mean time
          (Just effect) <- runDB $ get effectKey
          defaultLayout $ addWidget $(widgetFile "effects/show")
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
     Just (key, effect) -> do
       (res, form, encType, csrfHtml) <- runFormPost $ editFormlet effect
       eResult <- case res of
         FormMissing -> return (Left "Form is blank" :: Either String Effect)
         FormFailure errors -> do
           return (Left $ "There were some problems with the form")
         FormSuccess effect' -> do
           return (Right effect')
       case eResult of
         Left info' -> do
           let info = information info'
           defaultLayout $ addWidget $(widgetFile "effects/edit")
         Right effect -> do
           runDB $ replace key effect
           redirect RedirectSeeOther (ShowEffectR $ effectName effect)
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
postResultEffectR :: String -> Handler RepHtml
postResultEffectR name = do
  -- Get the effect from the database.
  mbResult <- runDB $ do { getBy $ UniqueEffect name }
  case mbResult of
    Just (_,effect) -> runEffect effect
    Nothing         -> effectNotFound name


runEffect :: Effect -> Handler RepHtml
runEffect effect = do
  foundation <- getYesod
  let imageFile' = imageFile (cacheDir foundation)

  -- Get the uploaded flie and save it to disk using its MD5 hash as the filename
  rr <- getRequest
  (_, files) <- liftIO $ reqRequestBody rr
  fi <- maybe notFound return $ lookup "file" files

  let contents     = fileContent fi
      imageInHash  = (base64md5 contents)
      imageInJpg   = imageInHash <.> "jpg"
      imageInBmp   = imageInHash <.> "bmp"

  liftIO $ BL.writeFile (imageFile' imageInJpg) contents

  -- Add the effect code to the wrapper, compile it and save the binary to disk
  let code          = effectCode effect
      codeHash      = base64md5 $ C.pack code
      codeDir       = (cacheDir foundation) </> "code"
      effectSrcFile = codeDir </> codeHash <.> "hs"
      effectExeFile = codeDir </> codeHash

  liftIO $ writeFile effectSrcFile $ (effectCodeWrapper foundation) ++ (effectCode effect)
  ret <- liftIO $ runProcess "ghc" True ["--make", effectSrcFile, "-o", effectExeFile] Nothing
  liftIO $ putStrLn ret

  -- Obtain the CUDA lock then run the effect. Use bitmap files as the intermediate
  -- image file format. Remove the bitmap files on completion.
  let imageOutHash = imageInHash ++ "-" ++ codeHash
      imageOutBmp  = imageOutHash <.> "bmp"
      imageOutJpg  = imageOutHash <.> "jpg"

  liftIO $ jpgToBmp (imageFile' imageInJpg) (imageFile' imageInBmp)
  liftIO $ withMVar (cudaLock foundation) $ \() -> do
    _ <- liftIO $ runProcess effectExeFile False [(imageFile' imageInBmp), (imageFile' imageOutBmp)] Nothing
    return ()
  liftIO $ bmpToJpg (imageFile' imageOutBmp) (imageFile' imageOutJpg)
  liftIO $ mapM removeFile $ map imageFile' [imageInBmp, imageOutBmp]

  -- Render both input and result images.
  defaultLayout $ do
    setTitle $ string $ fileName fi
    addWidget $(widgetFile "effects/result")



-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- "Backend" processing code.

-- | Run a separate process, redirecting stdout to the returned string.
--
runProcess :: FilePath
           -> Bool
           -> [String]
           -> Maybe [(String, String)]
           -> IO String
runProcess cmd path args env = do
  (outr, outw) <- createPipe
  cpid   <- forkProcess $ doProcess outw

  closeFd outw
  hr     <- fdToHandle outr
  outstr <- hGetContents hr
  _      <- getProcessStatus True False cpid

  return outstr

  where
    doProcess :: Fd -> IO ()
    doProcess outw = do
      dupTo outw stdOutput
      dupTo outw stdError
      executeFile cmd path args env


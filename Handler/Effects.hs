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
import System.Exit

import Yesod.Helpers.Static

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
  results <- runDB $ selectList [] [] 1000 0
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
  button <- lookupPostParam "SubmitCode"
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
           case button of
             (Just "Preview") -> do
               -- compile code
               compileRes <- compileEffect (effectCode effect)
               case compileRes of
                 (Left err) -> do
                   -- show error
                   let info = information err
                       preview = addHtml ""
                   defaultLayout $ addWidget $(widgetFile "effects/edit")
                 (Right binary) -> do
                   -- show preview
                   let info = information ("" :: String)
                   outPreview <- runEffect binary Settings.previewImage
                   defaultLayout $ do
                     addWidget $(widgetFile "effects/edit")
                     addWidget $(widgetFile "effects/preview")
             (Just "Save") -> do
               runDB $ replace key effect
               redirect RedirectSeeOther (ShowEffectR $ effectName effect)
             _ -> error "die - unknown submit button" -- FIXME: Need to handle this gracefully

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
    Just (_,effect) -> run effect
    Nothing         -> effectNotFound name

  where
    run :: Effect -> Handler RepHtml
    run effect = do
      foundation <- getYesod

      -- Get the uploaded flie and save it to disk using its MD5 hash as the filename
      rr <- getRequest
      (_, files) <- liftIO $ reqRequestBody rr
      fi <- maybe notFound return $ lookup "file" files

      let contents     = fileContent fi
          imageIn      = (base64md5 contents) <.> "jpg"
      liftIO $ BL.writeFile (imageFile (cacheDir foundation) imageIn) contents

      -- Add the effect code to the wrapper, compile it and save the binary to disk
      compileRes <- compileEffect (effectCode effect)
      case compileRes of
        (Left err)     -> error err   -- FIXME: need to handle this gracefully
        (Right binary) -> do
          imageOut <- runEffect binary imageIn
          -- Render both input and result images.
          defaultLayout $ do
            setTitle $ string $ fileName fi
            addWidget $(widgetFile "effects/result")



-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Helpers for compiling and running effects
--


-- | Compile the effect code. Return either the path to the compiled binary (Right) or
--   the compiler error (Left).
--
compileEffect :: String -> Handler (Either String FilePath)
compileEffect code = do
  foundation <- getYesod
  let codeHash      = base64md5 $ C.pack code
      codeDir       = (cacheDir foundation) </> "code"
      effectSrcFile = codeDir </> codeHash <.> "hs"
      effectExeFile = codeDir </> codeHash

  exists <- liftIO $ doesFileExist effectExeFile
  case exists of
    True  -> liftIO $ putStrLn "already exists" >> return (Right effectExeFile)
    False -> do
      liftIO $ writeFile effectSrcFile $ (effectCodeWrapper foundation) ++ code
      res <- liftIO $ runProcess "ghc" True ["--make", effectSrcFile, "-o", effectExeFile] Nothing

      case res of
        (Just output) -> return (Left output)
        Nothing       -> return (Right effectExeFile)


-- | Obtain the CUDA lock then run the effect. Use bitmap files as the intermediate
--   image file format. Remove the bitmap files on completion.
--
runEffect :: FilePath -> FilePath -> Handler FilePath
runEffect effectBin jpgImg = do
  foundation <- getYesod
  scratchDir <- liftIO $ getTemporaryDirectory

  let imageInHash  = (takeBaseName jpgImg)
      imageInJpg   = jpgImg
      imageOutJpg  = imageFile (cacheDir foundation) $ imageOutHash <.> "jpg"
      imageOutHash = imageInHash ++ "-" ++ (takeBaseName effectBin)
      imageInBmp   = scratchDir </> imageInHash <.> "bmp"
      imageOutBmp  = scratchDir </> imageOutHash <.> "bmp"

  liftIO $ jpgToBmp imageInJpg imageInBmp
  liftIO $ withMVar (cudaLock foundation) $ \() -> do
    _ <- liftIO $ runProcess effectBin False [imageInBmp, imageOutBmp] Nothing
    return ()
  liftIO $ bmpToJpg imageOutBmp imageOutJpg
  liftIO $ mapM removeFile [imageInBmp, imageOutBmp]

  return $ takeFileName imageOutJpg


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- "Backend" processing code.

-- | Run a separate process, redirecting stdout to the returned string.
--
runProcess :: FilePath
           -> Bool
           -> [String]
           -> Maybe [(String, String)]
           -> IO (Maybe String)
runProcess cmd path args env = do
  (outr, outw) <- createPipe
  cpid   <- forkProcess $ doProcess outw

  closeFd outw
  hr     <- fdToHandle outr
  outstr <- hGetContents hr
  status <- getProcessStatus True False cpid

  case status of
    (Just (Exited ExitSuccess)) -> return Nothing
    _                           -> return (Just outstr)

  where
    doProcess :: Fd -> IO ()
    doProcess outw = do
      dupTo outw stdOutput
      dupTo outw stdError
      executeFile cmd path args env


{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables, TypeOperators #-}
module Handler.Images where

-- standard libraries
import Control.Concurrent.MVar
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List
import System.IO
import System.Directory
import System.FilePath
import System.Cmd
import System.Posix.Types
import System.Posix.IO
import System.Posix.Process
import System.Exit

import Yesod.Helpers.Static


-- friends
import Foundation
import Settings
import Model


-- | Return the un-altered preview image.
--
getOriginalImageR :: Handler ()
getOriginalImageR = sendFile "image/jpeg" Settings.previewImage


-- | Return the "bug" image for generated images whose effect code doesn't compile.
--
getBugImageR :: Handler ()
getBugImageR = sendFile "image/jpeg" Settings.bugImage


-- | Retrieve preview image resources.
--
getPreviewImageR :: String -> Handler ()
getPreviewImageR name = do
  foundation <- getYesod
  mbResult   <- runDB $ do { getBy $ UniqueEffect name }

  case mbResult of
    Nothing         -> getBugImageR
    Just (_,effect) -> do
      let effectHash    = codeHash effect
          inputImgFile  = Settings.previewImage
          resultImgFile = imageFile (cacheDir foundation) ("preview-" ++ effectHash)

      getGeneratedImage effect inputImgFile resultImgFile


-- | Retrieve an image that has been uploaded.
--
getInputImageR :: String -> Handler ()
getInputImageR imgHash = do
  foundation <- getYesod
  sendFile "image/jpeg" $ imageFile (cacheDir foundation) imgHash


-- | Save an input image to disk.
--
saveInputImage :: FileInfo -> Handler FilePath
saveInputImage fi = do
  foundation <- getYesod
  let contents = fileContent fi
      imgHash  = imageHash fi
      filepath = imageFile (cacheDir foundation) imgHash
  liftIO $ BL.writeFile filepath contents
  return filepath


-- | Retrieve image resource that is the result of running an effect.
--
getResultImageR :: String -> String -> Handler ()
getResultImageR name inImgHash = do
  foundation <- getYesod
  mbResult   <- runDB $ do { getBy $ UniqueEffect name }

  case mbResult of
    Nothing         -> getBugImageR
    Just (_,effect) -> do
      let effectHash    = codeHash effect
          inputImgFile  = imageFile (cacheDir foundation) inImgHash
          resultImgFile = imageFile (cacheDir foundation) (effectHash ++ "-" ++ inImgHash)

      getGeneratedImage effect inputImgFile resultImgFile


-- | Retrive an image resource that is generated by running an effect. See
--   getResultImageR and getPreviewImageR.
--
getGeneratedImage :: Effect -> FilePath -> FilePath -> Handler ()
getGeneratedImage effect inImgFile outImgFile = do
  exists <- liftIO $ doesFileExist outImgFile
  case exists of
    True  -> sendFile "image/jpeg" outImgFile
    False -> do
      compileRes <- compileEffect effect
      case compileRes of
        (Left _)       -> getBugImageR
        (Right binary) -> do
          runEffect binary inImgFile outImgFile
          sendFile "image/jpeg" outImgFile



-- | For a given image "basename", return its location on disk.
--
imageFile :: FilePath -> String -> FilePath
imageFile cacheD imgName = cacheD </> "images" </> imgName <.> "jpg"


-- | Produce a hash string for an image file based on its contents.
--
imageHash :: FileInfo -> String
imageHash = base64md5 . fileContent


-- | Produce a hash of an effect's code string.
--
codeHash :: Effect -> String
codeHash effect = base64md5 $ C.pack (effectCode effect)


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Helpers for compiling and running effects
--

-- | Compile the effect code. Return either the path to the compiled binary (Right) or
--   the compiler error (Left).
--
compileEffect :: Effect -> Handler (Either String FilePath)
compileEffect effect = do
  foundation <- getYesod
  let hash          = codeHash effect
      codeDir       = (cacheDir foundation) </> "code"
      effectSrcFile = codeDir </> hash <.> "hs"
      effectExeFile = codeDir </> hash

  exists <- liftIO $ doesFileExist effectExeFile
  case exists of
    True  -> return (Right effectExeFile)
    False -> do
      liftIO $ writeFile effectSrcFile $ (effectCodeWrapper foundation) ++ (indent $ effectCode effect)
      res <- liftIO $ runProcess "ghc" True ["--make", effectSrcFile, "-o", effectExeFile] Nothing

      case res of
        (Just output) -> return (Left output)
        Nothing       -> return (Right effectExeFile)

  where
    indent = concat . intersperse "\n" . map ("    " ++) . lines


-- | Obtain the CUDA lock then run the effect. Use bitmap files as the intermediate
--   image file format. Remove the bitmap files on completion.
--
runEffect :: FilePath -> FilePath -> FilePath -> Handler ()
runEffect effectBin imageInJpg imageOutJpg = do
  foundation <- getYesod
  scratchDir <- liftIO $ getTemporaryDirectory
  request    <- getRequest

  let imageInBmp   = scratchDir </> "in"  <.> "bmp"
      imageOutBmp  = scratchDir </> "out" <.> "bmp"

  liftIO $ withMVar (cudaLock foundation) $ \() -> do
    liftIO $ jpgToBmp imageInJpg imageInBmp
    _ <- runProcess effectBin False [imageInBmp, imageOutBmp] Nothing
    liftIO $ bmpToJpg imageOutBmp imageOutJpg
  _ <- liftIO $ mapM removeFile [imageInBmp, imageOutBmp]

  return ()


-- | Convert a JPEG file to a bitmap file.
--
jpgToBmp :: FilePath -> FilePath -> IO ()
jpgToBmp jpgFile bmpFile = rawSystem "convert" [jpgFile, bmpFile] >> return ()


-- | Convert a bitmap file to a JPEG file.
--
bmpToJpg :: FilePath -> FilePath -> IO ()
bmpToJpg bmpFile jpgFile = rawSystem "convert" [bmpFile, jpgFile] >> return ()


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
      _ <- dupTo outw stdOutput
      _ <- dupTo outw stdError
      executeFile cmd path args env


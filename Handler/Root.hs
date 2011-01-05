{-# LANGUAGE TemplateHaskell, OverloadedStrings, ScopedTypeVariables #-}
module Handler.Root where

-- standard libraries
import Control.Applicative

-- friends
import Foundation

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Foundation.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
    (res, form, enctype) <- runFormGet $ uploadFileForm
    output <- case res of
      FormMissing   -> return ("You didn't specify a file" :: String)
      FormFailure _ -> return "Please correct the errors below."
      FormSuccess (Params imageFileName) -> do
        return "How you doing?"
    defaultLayout $ do
      setTitle "Recogniser!"
      addWidget $(widgetFile "homepage")

data Params = Params { imageFileName :: FileInfo }

uploadFileForm :: Form s m Params
uploadFileForm = fieldsToDivs $ Params <$> fileField "Image file"

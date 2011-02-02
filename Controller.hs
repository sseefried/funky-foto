{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withFoundation
    ) where

-- standard libraries
import Control.Monad
import Control.Concurrent.MVar
import System.Directory
import System.FilePath

-- friends
import Foundation
import Settings
import Yesod.Helpers.Static

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Effects
import Handler.Images

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Foundation" resourcesFoundation

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withFoundation :: (Application -> IO a) -> IO a
withFoundation f = Settings.withConnectionPool $ \p -> do
    lock    <- newMVar ()
    wrapper <- readFile "EffectWrapper.hs"
    cache   <- createCache

    let static = fileLookupDir Settings.staticdir typeByExt
        foundation = Foundation { getStatic         = static
                                , connPool          = p
                                , cudaLock          = lock
                                , effectCodeWrapper = wrapper
                                , cacheDir          = cache }

    toWaiApp foundation >>= f

  where
    -- Temporary directory where images and code are stored
    createCache = do
      cache <- getAppUserDataDirectory "funky-foto-cache"

      exists <- doesDirectoryExist cache
      when exists (removeDirectoryRecursive cache)

      createDirectory cache
      createDirectory $ cache </> "images"
      createDirectory $ cache </> "code"

      return cache

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

#include "settings.h"

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , widgetFile
    , connStr
    , ConnectionPool
    , withConnectionPool
    , runConnectionPool
    , approot
    , staticroot
    , staticdir
    , previewImage
    , bugImage
    , spinnerImage
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import Language.Haskell.TH.Syntax
import Database.Persist.Sqlite
import Yesod (MonadInvertIO, addWidget, addCassius, addJulius)
import Data.Monoid (mempty)
import System.Directory (doesFileExist)

-- | The base URL for your application. This will usually be different for
-- development and production. Yesod automatically constructs URLs for you,
-- so this value must be accurate to create valid links.
approot :: String
#ifdef PRODUCTION
approot = "http://funkyfotoapp.com"
#else
approot = "http://localhost:3000"
#endif

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticdir :: FilePath
staticdir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticroot :: String
staticroot = approot ++ "/static"

-- Location of image files to use for effect preivews.
--
previewImage :: FilePath
previewImage = "original.jpg"

bugImage :: FilePath
bugImage = "bug.jpg"

spinnerImage :: FilePath
spinnerImage = "spinner.gif"


-- | The database connection string. The meaning of this string is backend-
-- specific.
connStr :: String
#ifdef PRODUCTION
connStr = "production.db3"
#else
connStr = "debug.db3"
#endif

-- | Your application will keep a connection pool and take connections from
-- there as necessary instead of continually creating new connections. This
-- value gives the maximum number of connections to be open at a given time.
-- If your application requests a connection when all connections are in
-- use, that request will fail. Try to choose a number that will work well
-- with the system resources available to you while providing enough
-- connections for your expected load.
--
-- Also, connections are returned to the pool as quickly as possible by
-- Yesod to avoid resource exhaustion. A connection is only considered in
-- use while within a call to runDB.
connectionCount :: Int
connectionCount = 10

-- The rest of this file contains settings which rarely need changing by a
-- user.

-- The following three functions are used for calling HTML, CSS and
-- Javascript templates from your Haskell code. During development,
-- the "Debug" versions of these functions are used so that changes to
-- the templates are immediately reflected in an already running
-- application. When making a production compile, the non-debug version
-- is used for increased performance.
--
-- You can see an example of how to call these functions in Handler/Root.hs
--
-- Note: due to polymorphic Hamlet templates, hamletFileDebug is no longer
-- used; to get the same auto-loading effect, it is recommended that you
-- use the devel server.

toHamletFile, toCassiusFile, toJuliusFile :: String -> FilePath
toHamletFile x = "hamlet/" ++ x ++ ".hamlet"
toCassiusFile x = "cassius/" ++ x ++ ".cassius"
toJuliusFile x = "julius/" ++ x ++ ".julius"

hamletFile :: FilePath -> Q Exp
hamletFile = H.hamletFile . toHamletFile

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile = H.cassiusFile . toCassiusFile
#else
cassiusFile = H.cassiusFileDebug . toCassiusFile
#endif

juliusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
juliusFile = H.juliusFile . toJuliusFile
#else
juliusFile = H.juliusFileDebug . toJuliusFile
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = unlessExists toHamletFile hamletFile
    let c = unlessExists toCassiusFile cassiusFile
    let j = unlessExists toJuliusFile juliusFile
    [|addWidget $h >> addCassius $c >> addJulius $j|]
  where
    unlessExists tofn f = do
        e <- qRunIO $ doesFileExist $ tofn x
        if e then f x else [|mempty|]

-- The next two functions are for allocating a connection pool and running
-- database actions using a pool, respectively. It is used internally
-- by the scaffolded application, and therefore you will rarely need to use
-- them yourself.
withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool connStr connectionCount

runConnectionPool :: MonadInvertIO m => SqlPersist m a -> ConnectionPool -> m a
runConnectionPool = runSqlPool

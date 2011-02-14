{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module Foundation
    ( Foundation (..)
    , FoundationRoute (..)
    , resourcesFoundation
    , Handler
    , Widget
    , ImageSize(..)
    , module Yesod
    , module Settings
--    , module Model
    , StaticRoute (..)
    ) where

-- standard libraries
import Yesod
import Yesod.Helpers.Static
import System.Directory
import qualified Data.ByteString.Lazy as L
import Web.Routes.Site (Site (formatPathSegments))
import Control.Monad (unless)
import Text.Jasmine (minifym)
import Database.Persist.GenericSql
import Control.Concurrent.MVar
import Control.Monad (liftM)

-- friends
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)
import qualified Settings


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data Foundation = Foundation
    { getStatic         :: Static -- ^ Settings for static file serving.
    , connPool          :: Settings.ConnectionPool -- ^ Database connection pool.
    , cudaLock          :: MVar ()
    , effectCodeWrapper :: String
    , cacheDir          :: FilePath
    }

-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler Foundation Foundation

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget Foundation Foundation

-- An enumeration of the various sizes that the preview image resource is
-- respresented.
data ImageSize
  = Normal       -- ^Original image size
  | Thumb        -- ^Image size for list page
  | Mobile       -- ^Image size for iPhone app
  deriving (Show, Eq, Read)

instance SinglePiece ImageSize where
  toSinglePiece Normal  = "normal"
  toSinglePiece Thumb   = "thumb"
  toSinglePiece Mobile  = "mobile"

  fromSinglePiece "normal" = Right Normal
  fromSinglePiece "thumb"  = Right Thumb
  fromSinglePiece "mobile" = Right Mobile
  fromSinglePiece _        = Left "Unsupported image size"


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype FoundationRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route Foundation = FoundationRoute
-- * Creates the value resourcesFoundation which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- Foundation. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the FoundationRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "Foundation" [$parseRoutes|
/static                               StaticR                 Static getStatic

/favicon.ico                          FaviconR                GET
/robots.txt                           RobotsR                 GET

/                                     HomeR                   GET
/about                                AboutR                  GET

/images/static/#ImageSize/original    OriginalImageR          GET
/images/static/#ImageSize/bug         BugImageR               GET
/images/static/spinner                SpinnerImageR           GET
/images/results/#String/#String       ResultImageR            GET
/images/previews/#ImageSize/#String   PreviewImageR           GET
/images/inputs/#String                InputImageR             GET

/effects                              ListEffectsR            GET
/effects/create                       CreateEffectR           POST PUT
/effects/#String/show                 ShowEffectR             GET
/effects/#String/edit                 EditEffectR             GET
/effects/#String/update               UpdateEffectR           POST PUT
/effects/#String/delete               DeleteEffectR           POST DELETE
/effects/#String/run                  RunEffectR              GET
/effects/#String/result               ResultEffectR           POST

|]


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Foundation where
    approot _ = Settings.approot

    defaultLayout widget = do
      subRoute    <- getCurrentRoute
      subToMaster <- getRouteToMaster
      let masterRoute :: Maybe (Route Foundation)
          masterRoute = (liftM subToMaster) subRoute
          isAboutR = isAboutPage masterRoute

      mmsg  <- getMessage
      pc <- widgetToPageContent $ do
        widget
        addCassius $(Settings.cassiusFile "default-layout")
      hamletToRepHtml $(Settings.hamletFile "default-layout")

      where
        -- We only have two top-level sections: Effects and About. This is a
        -- simple way of determining which one we're in so we can highlight it
        -- correctly using the "selected" class.
        isAboutPage :: Maybe (Route Foundation) -> Bool
        isAboutPage (Just AboutR) = True
        isAboutPage _             = False

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ format s
      where
        format = formatPathSegments ss
        ss :: Site StaticRoute (String -> Maybe (GHandler Static Foundation ChooseRep))
        ss = getSubSite
    urlRenderOverride _ _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

-- How to run database actions.
instance YesodPersist Foundation where
    type YesodDB Foundation = SqlPersist
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db

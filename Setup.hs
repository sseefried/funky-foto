#! /usr/bin/env runhaskell

{-# LANGUAGE ExistentialQuantification, GADTs #-}

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription
import System.IO
import Network.Curl


main :: IO ()
main = defaultMainWithHooks customHooks
  where
    customHooks = simpleUserHooks {
      preBuild = mkDeployHeader
    }


-- |Create a C header file defining configuration parameters for this
-- instance of the web server.
--
mkDeployHeader :: Args -> BuildFlags -> IO HookedBuildInfo
mkDeployHeader _ _ = do
  hostname <- getHostname
  let port    = 3001
      approot = "http://" ++ hostname ++ ":" ++ (show port)
  let str =
          "/* Auto-generated by Setup.hs */\n" ++
          "#define CFG_APP_PORT        3001\n" ++
          "#define CFG_APP_ROOT_STR    \"" ++ approot ++ "\""

  writeFile "settings.h" str
  return emptyHookedBuildInfo


-- | Get the hostname of the instance we're running on. Query AWS for this
-- instances public hostname, but if we timeout (probably because we're running
-- locally), just use localhost.
--
getHostname :: IO String
getHostname = do
  let url = "http://169.254.169.254/latest/meta-data/public-hostname"
  (c, s) <- curlGetString url [CurlTimeout 2]
  case c of
    CurlOK -> return s
    _      -> return "localhost"


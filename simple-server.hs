{-# LANGUAGE CPP #-}

#include "settings.h"

import Controller
import Network.Wai.Handler.SimpleServer (run)

main :: IO ()
main = putStrLn "Loaded" >> withFoundation (run CFG_APP_PORT)

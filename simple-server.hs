{-# LANGUAGE CPP #-}

#include "settings.h"

-- standard libraries
import Network.Wai.Handler.SimpleServer (run)
import Database.Persist.GenericSql(runSqlPool, runMigration)

-- friends
import Model(migrateAll)
import Controller
import Settings(withConnectionPool)

main :: IO ()
main = do
  withConnectionPool $ \pool -> runSqlPool (runMigration migrateAll) pool
  putStrLn "Loaded" >> withFoundation (run 5000)

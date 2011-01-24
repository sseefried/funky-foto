-- standard libraries
import Network.Wai.Handler.DevelServer (run)
import Control.Concurrent (forkIO)
import Database.Persist.GenericSql(runSqlPool, runMigration)

-- friends
import Model(migrateAll)
import Settings(withConnectionPool)

main :: IO ()
main = do
    withConnectionPool $ \p -> runSqlPool (runMigration migrateAll) p
    mapM_ putStrLn
        [ "Starting your server process. Code changes will be automatically"
        , "loaded as you save your files. Type \"quit\" to exit."
        , "You can view your app at http://localhost:3000/"
        , ""
        ]
    _ <- forkIO $ run 3000 "Controller" "withFoundation" ["hamlet"]
    go
  where
    go = do
        x <- getLine
        case x of
            'q':_ -> putStrLn "Quitting, goodbye!"
            _ -> go

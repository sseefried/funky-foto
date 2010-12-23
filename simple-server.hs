import Controller
import Network.Wai.Handler.SimpleServer (run)

main :: IO ()
main = putStrLn "Loaded" >> withFoundation (run 3000)

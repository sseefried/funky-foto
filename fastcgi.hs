import Controller
import Network.Wai.Handler.FastCGI (run)

main :: IO ()
main = withFoundation run

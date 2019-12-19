import Data.List.Split
import System.Environment
import Intcode
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        icm = initIntcode (map (read :: String -> Int) . splitOn "," $ s) []
        r = fetchSquare icm (0, 0) (100, 100)
    in do {
        putStrLn r;
        print (length . filter (\x -> x == '#') $ r)
    }
}

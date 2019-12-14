import Data.List.Split
import Data.Map (filter, empty)
import System.Environment
import Control.Lens
import Control.Lens.Operators
import Intcode
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        t = (map (read :: String -> Int) . splitOn "," $ s)
        Cabinet{display=o} = runCabinet Cabinet{state=initIntcode ((element 0 .~ 2) t) [], display=empty}
    in
    putStrLn . showMap $ o;
}

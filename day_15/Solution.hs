import Data.List.Split
import Data.Map (singleton)
import System.Environment
import Intcode
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        rb = singleton (0, 0) (Empty, Just (initIntcode (map (read :: String -> Int) . splitOn "," $ s) []))
        o = runRepairBot rb
    in do {
        putStrLn (showTiles o);
        putStrLn ("Distance to oxygen system: " ++ (show (distance o)));
        putStrLn ("Time to spread oxygen: " ++ (show (fill o)));
    }
}

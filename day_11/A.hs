import Data.List.Split
import System.Environment
import Paintbot

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        tape = (map (read :: String -> Int) . splitOn "," $ s)
        pb = newPaintbot tape []
        final = runPaintbot pb
    in
    print (length (tiles final));
}

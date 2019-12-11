import Data.List.Split
import Data.Map (Map, empty, insert, findWithDefault, keys)
import System.Environment
import Paintbot

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        tape = (map (read :: String -> Int) . splitOn "," $ s)
        pb = newPaintbot tape [((0, 0) :: Position, White)]
        final = runPaintbot pb
    in
    putStr (showTiles (tiles (final)));
}

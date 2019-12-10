import System.Environment
import Data.Sort
import Data.Ord
import Data.List.Extras.Argmax
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        q = readAsteroids s
        o = (23, 19)
        g = angleGrouping o q
    in
    print ((vaporizeOrder g) !! 199);
}

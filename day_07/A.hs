import Data.List.Split
import Data.List
import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        tape = map (read :: String -> Int) . splitOn "," $ s
    in
    print (maximum [processSequence tape x 0 | x <- permutations [4,3,2,1,0]]);
}

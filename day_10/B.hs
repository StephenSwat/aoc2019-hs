import System.Environment
import Data.Sort
import Data.Ord
import Data.List.Extras.Argmax
import Common

vaporizeOrder :: [[Asteroid]] -> [Asteroid]
vaporizeOrder [] = []
vaporizeOrder (x:xs) = [head x] ++ vaporizeOrder (xs ++ if null (tail x) then [] else [tail x])

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        q = readAsteroids s
        o = (23, 19)
        g = angleGrouping o q
        p = map (reverse . sortBy (comparing (sqLen . difference o))) g
    in
    print ((vaporizeOrder p) !! 199);
}

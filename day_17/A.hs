import Data.List.Split
import Data.Char (chr)
import System.Environment
import Intcode
import Cardinal
import Common

intersections :: String -> [(Int, Int)]
intersections x = [p | x <- xr, y <- yr, let p = (x, y), isIntersection p]
    where
        isScaffolding :: (Int, Int) -> Bool
        isScaffolding (x, y) = ((l !! y) !! x) == '#'

        isIntersection :: (Int, Int) -> Bool
        isIntersection p = all (isScaffolding . (addTuples p)) [(0, 0), (0, 1), (0, -1), (1, 0), (-1, 0)]

        l = filter ((> 0) . length) . lines $ x
        yr = [1..(length l) - 2]
        xr = [1..(length (head l)) - 2]

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        i = initIntcode (map (read :: String -> Int) . splitOn "," $ s) [];
        IntcodeState{output=o} = runProgram i;
        l = map chr o;
    in do {
        putStrLn l;
        print(sum . map (\(x, y) -> x * y) . intersections $ l)
    }
}

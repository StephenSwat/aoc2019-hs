import System.Environment
import Data.List
import Common

bsearch :: (Int -> Bool) -> Int -> Int -> Int
bsearch f a b
    | (b - a) <= 1 = a
    | otherwise = bsearch f na nb
    where
        m = a + ((b - a) `div` 2)
        fm = f m
        na = if fm then m else a
        nb = if fm then b else m

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        r = readRecipes s;
        q :: Int -> Bool
        q x = a <= 1000000000000
            where Just (_, a) = find (\(n, _) -> n == "ORE") (produce r ("FUEL", x))
    in
    print (bsearch q 0 10000000000000)
}

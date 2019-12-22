import System.Environment
import Data.Map ((!), fromList)
import Data.List
import Common

applyShuffle :: [Int] -> Shuffle -> [Int]
applyShuffle d (Reverse) = reverse d
applyShuffle d (Cut c)
    | c < 0 = applyShuffle d (Cut (length d + c))
    | otherwise = (drop c d) ++ (take c d)
applyShuffle d (Deal c) = map (m !) [0..l - 1]
    where
        l = length d
        m = fromList [((i * c) `mod` l, d !! i) | i <- [0..l - 1]]

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        i = parseShuffles s;
        d = [0..10006]
        -- r = foldl applyShuffle d i
        r = foldl applyShuffle [0..9] [Cut (-2)]
    in do {
        print r;
        -- print (findIndex (== 2019) r);
    }
}

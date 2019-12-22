import System.Environment
import Data.Map ((!), fromList)
import Data.List
import Common

applyShuffle :: [Integer] -> Shuffle -> [Integer]
applyShuffle d (Reverse) = reverse d
applyShuffle d (Cut c)
    | c < 0 = applyShuffle d (Cut (fromIntegral (length d) + c))
    | otherwise = (drop (fromIntegral c) d) ++ (take (fromIntegral c) d)
applyShuffle d (Deal c) = map (m !) [0..l - 1]
    where
        l = length d
        m = fromList [((i * (fromInteger c)) `mod` l, d !! i) | i <- [0..l - 1]]

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        i = parseShuffles s;
        d = [0..10006]
        r = foldl applyShuffle d i
    in do {
        print (findIndex (== 2019) r);
    }
}

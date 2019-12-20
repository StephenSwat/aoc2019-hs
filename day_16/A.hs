import System.Environment
import Common

multiplierList :: Int -> [Int]
multiplierList n = tail (cycle ((q 0) ++ (q 1) ++ (q 0) ++ (q (-1))))
    where
        l = n + 1
        q = (replicate l)

fft :: [Int] -> [Int]
fft n = map q [0..(length n) - 1]
    where
        q x = (`mod` 10) . abs . sum $ (zipWith (*) n (multiplierList x))

main = do {
    [f] <- getArgs;
    s <- readFile f;
    print (digitsToInt . take 8 . (!! 100) . iterate fft . intToDigits . read $ s);
}

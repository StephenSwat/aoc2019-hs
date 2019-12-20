import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        n = concat . (replicate 10000) . intToDigits . read $ s;
        o = digitsToInt . take 7 $ n
    in
    print (digitsToInt . take 8 . (!! 100) . iterate (scanr (\x y -> (x + y) `mod` 10) 0) . drop o $ n);
}

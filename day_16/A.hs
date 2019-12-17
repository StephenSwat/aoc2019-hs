import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        n = intToDigits . read $ s;
        x = iterate fft n;
    in
    print (digitsToInt . take 8 . (!! 100) $ x);
}

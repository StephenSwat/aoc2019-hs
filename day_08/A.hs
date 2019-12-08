import System.Environment
import Data.List.Extras.Argmax
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        lrs = layers (getImage s) (6, 25)
        bl = argmin (\x -> length [y | y <- x, y == 0]) lrs
    in
    print ((length [x | x <- bl, x == 1]) * (length [x | x <- bl, x == 2]));
}

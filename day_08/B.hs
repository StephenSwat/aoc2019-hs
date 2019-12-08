import System.Environment
import Data.List.Extras.Argmax
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let
        lrs = layers (getImage s) (6, 25)
    in
    putStr (showImage (render lrs) (6, 25))
}

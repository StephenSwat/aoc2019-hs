import System.Environment
import Data.List.Extras.Argmax
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        q = readAsteroids s
        (a, m) = argmaxWithMax (\x -> length (angleGrouping x [y | y <- q, x /= y])) q
    in
    putStrLn ((show a) ++ " " ++ (show m));
}

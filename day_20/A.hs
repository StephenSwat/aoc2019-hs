import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        m = parseMaze s;
        Just p = solveMaze m False;
    in do {
        print (length p)
    }
}

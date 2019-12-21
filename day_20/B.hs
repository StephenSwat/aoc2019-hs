import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        m = parseMaze s;
        Just p = solveMaze m True;
    in do {
        print (length p)
    }
}

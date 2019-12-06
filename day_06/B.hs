import Data.Tree (drawTree)
import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        q = toTree . parseString $ s
    in
    print ((shortestPath q "YOU" "SAN") - 2);
}
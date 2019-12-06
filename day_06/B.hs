import Data.Tree (drawTree)
import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        q = toTree . parseString $ s
        r = deepestCommonAncestor q "YOU" "SAN"
    in
    print (shortestPath r "YOU" "SAN");
}
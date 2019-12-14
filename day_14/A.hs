import System.Environment
import Data.List
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        r = readRecipes s;
        Just l = find (\(n, _) -> n == "ORE") (produce r ("FUEL", 1))
    in
    print l;
}

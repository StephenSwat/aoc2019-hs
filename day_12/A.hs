import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        q = parseString s;
        r = iterate timeStep q
        f = r !! 1000
    in
    print (systemEnergy f);
}

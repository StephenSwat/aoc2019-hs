import System.Environment
import Data.List
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        q = parseString s;
        (ix, iy, iz) = horizontal q;
        r = iterate timeStep q

        Just fx = findIndex (\i -> let (f, _, _) = horizontal i in f == ix) (tail r)
        Just fy = findIndex (\i -> let (_, f, _) = horizontal i in f == iy) (tail r)
        Just fz = findIndex (\i -> let (_, _, f) = horizontal i in f == iz) (tail r)
    in
    print (lcm (fx + 1) (lcm (fy + 1) (fz + 1)));
}

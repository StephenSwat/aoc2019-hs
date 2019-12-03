import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let l = lines s
        l1 = chains $ findPoints $ parseLine (l !! 0)
        l2 = chains $ findPoints $ parseLine (l !! 1)
        i = concat [[(init a ++ [l], init b ++ [l]) | l <- intersection (lastTwo a) (lastTwo b)] | a <- l1, b <- l2]
        q = [l | (a, b) <- i, let l = (chainLength a) + (chainLength b), l > 0]
    in 
    print (minimum q)
}



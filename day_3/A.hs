import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let l = lines s
        l1 = pointPairs $ findPoints $ parseLine (l !! 0)
        l2 = pointPairs $ findPoints $ parseLine (l !! 1)
        i = (concat . map (\(a, b) -> intersection a b)) [(a, b) | a <- l1, b <- l2]
    in 
    print (minimum [manhattan x | x <- i, manhattan x > 0])
}



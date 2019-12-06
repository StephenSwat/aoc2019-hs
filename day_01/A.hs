import System.Environment

main = do {
    [f] <- getArgs;
    s <- readFile f;
    print (sum . map (\x -> x `div` 3 - 2) . map (read :: String -> Integer) . lines $ s);
}

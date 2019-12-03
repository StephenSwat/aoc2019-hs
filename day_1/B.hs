import System.Environment

getFuel :: Integer -> Integer
getFuel mass = max 0 (mass `div` 3 - 2)

propagateFuel :: [Integer] -> [Integer]
propagateFuel x
    | all (== 0) n = []
    | otherwise = n ++ propagateFuel n
    where n = map getFuel x

main = do {
    [f] <- getArgs;
    s <- readFile f;
    print (sum . propagateFuel . map (read :: String -> Integer) . lines $ s);
}

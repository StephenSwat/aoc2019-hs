import Common

main = do {
    print $ length [x | x <- [236491..713787], let y = sixDigits x, adjacentDigits y 2 (==), isIncreasing y];
}
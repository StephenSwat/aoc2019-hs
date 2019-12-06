module Common where

replace :: [Int] -> Int -> Int -> [Int]
replace l i n = take i l ++ [n] ++ drop (i + 1) l

runProgramHelper :: [Int] -> Int -> [Int]
runProgramHelper x c
    | [1,a,b,z] <- op = runProgramHelper (replace x z (x !! a + x !! b)) (c + 4)
    | [2,a,b,z] <- op = runProgramHelper (replace x z (x !! a * x !! b)) (c + 4)
    | (99:xs) <- op = x
    | otherwise = error "Bad input"
    where 
        op = (take 4 . drop c) x
        
runProgram :: [Int] -> [Int]
runProgram x = runProgramHelper x 0

module Common where

replaceTape :: [Int] -> Int -> Int -> [Int]
replaceTape l i n = take i l ++ [n] ++ drop (i + 1) l

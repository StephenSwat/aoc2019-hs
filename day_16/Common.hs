module Common where

digitsToInt :: [Int] -> Int
digitsToInt ([]) = 0
digitsToInt (x) = (last x) + 10 * (digitsToInt (init x))

intToDigits :: Integer -> [Int]
intToDigits 0 = []
intToDigits n = (intToDigits q) ++ [fromInteger r]
    where
        (q, r) = divMod n 10
    
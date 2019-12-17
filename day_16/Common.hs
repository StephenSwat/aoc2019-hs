module Common where

digitsToInt :: [Int] -> Integer
digitsToInt ([]) = 0
digitsToInt (x) = (toInteger (last x)) + 10 * (digitsToInt (init x))

intToDigits :: Integer -> [Int]
intToDigits 0 = []
intToDigits n = (intToDigits q) ++ [fromInteger r]
    where
        (q, r) = divMod n 10
    
multiplierList :: Int -> [Int]
multiplierList n = tail (cycle ((q 0) ++ (q 1) ++ (q 0) ++ (q (-1))))
    where
        l = n + 1
        q = (replicate l)

fft :: [Int] -> [Int]
fft n = map q [0..(length n) - 1]
    where
        q x = (`mod` 10) . abs . sum $ (zipWith (*) n (multiplierList x))

module Digits where

digitsToInt :: (Integral a, Integral b) => [a] -> b
digitsToInt ([]) = 0
digitsToInt (x) = fromIntegral ((last x) + 10 * (digitsToInt (init x)))

intToDigits :: (Integral a, Integral b) => a -> [b]
intToDigits 0 = []
intToDigits n = (intToDigits . fromIntegral $ q) ++ [fromIntegral r]
    where
        (q, r) = divMod n 10
    
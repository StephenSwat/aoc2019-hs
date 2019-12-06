module Common where 

-- Given an integer, construct an array of six single-digits integers that
-- contains each of the six least significant digits of the input.
-- sixDigits 2267 = [0,0,2,2,6,7]
sixDigits :: Integer -> [Integer]
sixDigits z = foldl (\x y -> [(z `div` (10^y)) `mod` 10] ++ x) [] [0..5]

-- Given a series of integers, return true if each element is always smaller
-- than or equal to the next.
-- isIncreasing [1,2,2,5] = True
-- isIncreasing [1,4,2,2,5] = False
isIncreasing :: [Integer] -> Bool
isIncreasing ([]) = True
isIncreasing (x:[]) = True
isIncreasing (x:y:xs) = (x <= y) && isIncreasing (y : xs)

-- Given any list, pack elements together into sub-lists containing identical
-- elements.
-- pack [1,1,5,7,7,7,1,1] = [[1,1], [5], [7,7,7], [1,1]]
pack :: Eq elem => [elem] -> [[elem]]
pack [] = []
pack (x:[]) = [[x]]
pack (x:xs)
    | head (head p) == x = [(head p) ++ [x]] ++ tail p
    | otherwise = [[x]] ++ p
    where p = pack xs

-- Create a run-length encoding of the a list of elements, returning tuples of
-- how many times in a row a certain element occurred.
-- rle [1,1,5,7,7,7,1,1] = [(2, 1), (1, 5), (3, 7), (1, 1)]
rle :: Eq elem => [elem] -> [(Int, elem)]
rle = (map (\x -> (length x, head x))) . pack

-- Given a list of numbers, a number n, and a comparison function, return
-- whether any element in the run-length encoding of the list has a length that
-- matches n under the given function.
adjacentDigits :: [Integer] -> Integer -> (Integer -> Integer -> Bool) -> Bool
adjacentDigits l n f = any (\(x, _) -> (toInteger x) `f` n) (rle l)

module Common where

import Data.List
import Data.List.Split

type Reagent = (String, Int)
type Recipe = ([Reagent], Reagent)

readReagent :: String -> Reagent
readReagent s = (n, read c)
    where
        (c:n:[]) = splitOn " " s

readRecipe :: String -> Recipe
readRecipe s = (map readReagent (splitOn ", " rea), readReagent res)
    where
        (rea:res:[]) = splitOn " => " s

readRecipes :: String -> [Recipe]
readRecipes = map readRecipe . lines 

squash :: [Reagent] -> [Reagent]
squash = filter f . map q . groupBy g . sort
    where
        g (n1, _) (n2, _) = n1 == n2
        q x = let (n, _) = head x in (n, sum [y | (_, y) <- x])
        f (_, c) = c /= 0

reduceReagent :: [Recipe] -> Reagent -> [Reagent]
reduceReagent r n@("ORE", _) = []
reduceReagent r (rn, rc) = z
    where
        Just (f, (n, c)) = find (\(_, (n, _)) -> n == rn) r
        pc = ceiling ((fromIntegral rc) / (fromIntegral c))
        z = (n, -(pc*c)):map (\(n', c') -> (n', c' * pc)) f

reduceReactor :: [Recipe] -> [Reagent] -> [Reagent]
reduceReactor r q
    | all (\(n, c) -> c <= 0 || n == "ORE") q = q
    | otherwise = reduceReactor r aq
    where
        aq = squash . (q ++) . concat . map (reduceReagent r) . filter (\(_, c) -> c >= 0) $ q

produce :: [Recipe] -> Reagent -> [Reagent]
produce r x = reduceReactor r [x]

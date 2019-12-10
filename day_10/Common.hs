module Common where
    
import Data.Sort
import Data.Ord
import Data.List.GroupBy
    
type Asteroid = (Int, Int)

sqLen :: (Int, Int) -> Int
sqLen (a, b) = a * a + b * b

difference :: Asteroid -> Asteroid -> (Int, Int)
difference (xo, yo) (xt, yt) = (xt - xo, yt - yo)

angle :: RealFloat a => Asteroid -> Asteroid -> a
angle a b = -((atan2 (fromIntegral dx) (fromIntegral dy)) - 0.5 * pi)
    where
        (dx, dy) = difference a b

equalAngle :: Asteroid -> Asteroid -> Asteroid -> Bool
equalAngle o p1 p2 = (angle o p1) == (angle o p2)

readAsteroids :: String -> [Asteroid]
readAsteroids x = [(x, y) | x <- [0..w-1], y <- [0..h-1], (l !! y) !! x == '#']
    where
        l = lines x
        h = length l
        w = length (head l)
        
angleGrouping :: Asteroid -> [Asteroid] -> [[Asteroid]]
angleGrouping o l = groupBy (equalAngle o) (sortBy (comparing (angle o)) l)
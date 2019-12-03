module Common where

import Data.List.Split

data Direction = North | South | West | East deriving (Show, Eq)

data Section = Section {
    dir :: Direction,
    len :: Integer
} deriving Show

type Point = (Integer, Integer)

makeSection :: String -> Section
makeSection (p:xs)
    | p == 'U' = Section North d
    | p == 'D' = Section South d
    | p == 'L' = Section West d
    | p == 'R' = Section East d
    where d = (read xs :: Integer)

parseLine :: String -> [Section]
parseLine = (map makeSection . splitOn ",") 

addSection :: Point -> Section -> Point
addSection (x, y) Section {dir=d, len=l}
    | d == North = (x, y + l)
    | d == South = (x, y - l)
    | d == West = (x - l, y)
    | d == East = (x + l, y)

findPoints :: [Section] -> [Point]
findPoints = foldl (\x y -> x ++ [addSection (last x) y]) [(0, 0)]

points :: (Point, Point) -> [Point]
points ((x1, y1), (x2, y2)) = [(x, y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]

intersection :: (Point, Point) -> (Point, Point) -> [Point]
intersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = q
    where xr = [(max (min x1 x2) (min x3 x4))..(min (max x1 x2) (max x3 x4))]
          yr = [(max (min y1 y2) (min y3 y4))..(min (max y1 y2) (max y3 y4))]
          q = [(x, y) | x <- xr, y <- yr]

chains :: [Point] -> [[Point]]
chains x = map (\n -> take n x) [2..length x]

pointPairs :: [Point] -> [(Point, Point)]
pointPairs [] = []
pointPairs (a:[]) = []
pointPairs (a:xs) = [(a, head xs)] ++ pointPairs xs

manhattan :: Point -> Integer
manhattan (a,b) = (abs a) + (abs b)

manhattanDistance :: Point -> Point -> Integer
manhattanDistance (x1, y1) (x2, y2) = manhattan (x1 - x2, y1 - y2)

lastTwo :: [Point] -> (Point, Point)
lastTwo x = (a, b) where [a,b] = drop ((length x) - 2) x

chainLength :: [Point] -> Integer
chainLength = (foldl (\x (a, b) -> x + manhattanDistance a b) 0 . pointPairs)


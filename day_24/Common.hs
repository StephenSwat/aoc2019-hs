module Common where

import Data.List
import System.Environment
import Cardinal
import Data.Map (Map, fromList, mapWithKey, findWithDefault, (!), mapKeys)
import Data.Set (Set, empty, member, insert)

data Tile = Empty | Bug deriving (Show, Eq, Ord)
type Grid = Map (Int, Int) Tile
type RecursiveGrid = Map (Int, Int, Int) Tile

charToTile :: Char -> Tile
charToTile '.' = Empty
charToTile '#' = Bug

tileToChar :: Tile -> Char
tileToChar Empty = '.'
tileToChar Bug = '#'

toRecursive :: Grid -> RecursiveGrid
toRecursive = mapKeys (\(x, y) -> (0, x, y))

gridFromString :: String -> Grid
gridFromString s = fromList [
        ((x, y), v) | 
        x <- [0..4], 
        y <- [0..4], 
        let v = charToTile . (!! x) . (!! y) . lines $ s
    ]

showGrid :: Grid -> String
showGrid s = intercalate "\n" [[tileToChar (findWithDefault Empty (x, y) s) | x <- [0..4]] | y <- [0..4]]

stepTile :: Int -> Tile -> Tile
stepTile n e
    | n == 1 = Bug
    | e == Empty && n == 2 = Bug
    | otherwise = Empty

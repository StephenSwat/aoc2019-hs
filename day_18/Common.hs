module Common where

import Data.List
import Data.Char (toLower, toUpper)
import Data.Map (Map, keys, findWithDefault, keys)
import Data.Set (Set)
import Cardinal

data Tile = Empty | Wall | Origin Int | Key Char | Door Char deriving (Show, Eq, Ord)
type Tiles = Map Position Tile
type Maze = Map Tile [(Tile, Int, Set Char)]

tileToChar :: Tile -> Char
tileToChar Empty = ' '
tileToChar Wall = '#'
tileToChar (Origin x) = last . show $ x
tileToChar (Key x) = toLower x
tileToChar (Door x) = toUpper x

showTiles :: Tiles -> String
showTiles t = intercalate "\n" [[tileToChar (findWithDefault Empty (x, y) t) | x <- [0..maxX]] | y <- [0..maxY]]
    where
        maxX = maximum . map fst . keys $ t
        maxY = maximum . map snd . keys $ t

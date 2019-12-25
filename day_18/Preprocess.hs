module Preprocess where

import Data.List.Extra (sortOn, groupOn)
import Data.Maybe (isJust, fromJust)
import Data.Map (Map, fromList, fromListWith, toList, (!), findWithDefault)
import Data.Set (Set, isSubsetOf, insert, delete, notMember, unions, empty, member)
import Data.Char (toLower, isLower)

import Cardinal

import Common

type Path = (Int, Set Char)

isInteresting :: Tile -> Bool
isInteresting (Key _) = True
isInteresting (Origin _) = True
isInteresting _ = False

isPassable :: Tile -> Bool
isPassable (Key _)  = True
isPassable (Door _) = True
isPassable (Origin _) = True
isPassable (Empty)  = True
isPassable (Wall)   = False

readTile :: Char -> Tile
readTile '.' = Empty
readTile '@' = Origin 0
readTile '#' = Wall
readTile x
    | isLower x = Key x
    | otherwise = Door (toLower x)

stringToTiles :: String -> Tiles
stringToTiles s = fromList [(p, readTile . (!! x) . (!! y) . lines $ s) | p@(x, y) <- coords]
    where
        l = lines s
        coords = [(x, y) | x <- [0..(length (head l))-1], y <- [0..(length l)-1]]

tilesToMaze :: Tiles -> Maze
tilesToMaze q = fromListWith (++) z
    where
        i = filter (isInteresting . snd) . toList $ q
        z = [(v1, [(v2, pl, pk) | (p2, v2) <- i, p1 /= p2, (pl, pk) <- shortestPaths q p1 p2]) | (p1, v1) <- i]

allPaths :: Tiles -> Position -> Position -> Int -> Set Position -> Set Char -> [Path]
allPaths m p1 p2 d v k
    | p1 == p2 = [(d, k)]
    | null moves = []
    | otherwise = concat . map (\x -> allPaths m x p2 (d + 1) nv nk) $ moves
    where
        t = m ! p1
        nv = insert p1 v
        nk = case t of
            Door x -> insert x k
            _      -> k

        resolvePosition :: Position -> Maybe Position
        resolvePosition np
            | member np nv = Nothing
            | isPassable (findWithDefault Wall p1 m) = Just np
            | otherwise = Nothing

        moves = map fromJust . filter isJust . map resolvePosition . map (addTuples p1) . map movementTuple $ [North, West, South, East]

shortestPaths :: Tiles -> Position -> Position -> [Path]
shortestPaths m p1 p2 = r
    where
        p = allPaths m p1 p2 0 empty empty
        s = map head . map (sortOn fst) . groupOn snd . sortOn snd $ p
        r = filter (\(l1, k1) -> all (\(l2, k2) -> l1 <= l2 || k1 `isSubsetOf` k2) s) s

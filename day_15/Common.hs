module Common where

import Intcode
import Debug.Trace
import Data.List
import Data.Maybe
import Data.Map (Map, (!), findWithDefault, keys, singleton, insert)

type Position = (Int, Int)

data Direction = North | West | South | East deriving (Show)
data Tile = Unknown | Empty | Wall | Goal deriving (Show, Eq)

type RepairBot = Map Position (Tile, Maybe IntcodeState)

tileToChar :: Tile -> Char
tileToChar Unknown = '?'
tileToChar Empty   = '.'
tileToChar Wall    = '#'
tileToChar Goal    = '$'

move :: Position -> Direction -> Position
move (x, y) North = (x, y + 1)
move (x, y) West  = (x - 1, y)
move (x, y) South = (x, y - 1)
move (x, y) East  = (x + 1, y)

dirToInt :: Direction -> Int
dirToInt North = 1
dirToInt South = 2
dirToInt West  = 3
dirToInt East  = 4

intToTile :: Int -> Tile
intToTile 0 = Wall
intToTile 1 = Empty
intToTile 2 = Goal
intToTile x = error ("Invalid return code " ++ (show x))

passable :: Tile -> Bool
passable Empty = True
passable Goal  = True
passable _     = False

getTile :: RepairBot -> Position -> (Tile, Maybe IntcodeState)
getTile r p = findWithDefault (Unknown, Nothing) p r

getLiberties :: RepairBot -> Position -> [Direction]
getLiberties t p = q
    where
        f x = let (y, _) = getTile t (move p x) in y == Unknown
        q = filter f $ [North, West, South, East]

tryLiberties :: RepairBot -> Position -> [Direction] -> RepairBot
tryLiberties r _ [] = r
tryLiberties r p (d:ds) = tryLiberties (Data.Map.insert (move p d) (ot, nt) r) p ds
    where
        (_, Just s) = r ! p
        (t, (o:[])) = takeAllOutput . runProgramUntilNeedInput . (addInput s) $ [dirToInt d]
        ot = intToTile o
        nt = case ot of
            Wall -> Nothing
            _    -> Just t

runRepairBot :: RepairBot -> RepairBot
runRepairBot r
    | isNothing t = r
    | otherwise = runRepairBot (tryLiberties r q (getLiberties r q))
    where
        valid :: Position -> Bool
        valid p = length (getLiberties r p) > 0 && let (z, _) = getTile r p in (passable z)

        t = (find valid) . keys $ r
        Just q = t

showTiles :: RepairBot -> String
showTiles t = intercalate "\n" lines
    where
        k = keys t
        minx = minimum [x | (x, _) <- k]
        maxx = maximum [x | (x, _) <- k]
        miny = minimum [y | (_, y) <- k]
        maxy = maximum [y | (_, y) <- k]
        lines = [
                [if (x, y) == (0, 0) then 'X' else tileToChar q | x <- [minx..maxx], let (q, _) = getTile t (x, y)]
            | y <- reverse [miny..maxy]]
            
distanceHelper :: RepairBot -> Position -> Position -> [Position] -> Maybe Int
distanceHelper r p1 p2 b
    | p1 == p2 = Just 0
    | null v = Nothing
    | otherwise = Just (1 + minimum v)
    where 
        f x = (passable . fst . getTile r $ x) && not (elem x b) 
        q = filter f . map (move p1) $ [North, West, South, East]
        v = [let Just e = d in e | x <- q, let d = distanceHelper r x p2 (p1:b), isJust d]

distance :: RepairBot -> Int
distance t = d
    where
        Just pg = (find (\x -> let (z, _) = getTile t x in z == Goal)) . keys $ t
        Just d = distanceHelper t (0, 0) pg []

fillHelper :: RepairBot -> Position -> [Position] -> Int
fillHelper r p b
    | null v = 0
    | otherwise = 1 + maximum v
    where 
        f x = (passable . fst . getTile r $ x) && not (elem x b) 
        q = filter f . map (move p) $ [North, West, South, East]
        v = [fillHelper r x (p:b) | x <- q]

fill :: RepairBot -> Int
fill t = fillHelper t pg []
    where
        Just pg = (find (\x -> let (z, _) = getTile t x in z == Goal)) . keys $ t

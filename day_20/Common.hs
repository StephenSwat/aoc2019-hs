module Common where

import Data.Maybe
import Data.List.Extras.Argmax
import Data.List
import qualified Data.Map (map)
import Data.Map (Map, fromList, fromListWith, filterWithKey, keys, (!), findWithDefault, assocs)
import Data.Set (Set, empty, insert, notMember)
import Data.Char (isLetter)
import Cardinal

data Tile = Path | Wall | Void deriving (Show, Eq)
data MovementType = Move | Portal deriving (Show, Eq, Ord)

type Move = (Position, Int, MovementType)

data Maze = Maze {
    tiles :: Map Position Tile,
    portals :: Map Position [Position],
    start :: Position,
    end :: Position,
    height :: Int,
    width :: Int
} deriving (Show)

data MazeState = MazeState {
    position :: Position, 
    layer :: Int,
    path :: [Move],
    visited :: Set Move,
    recurse :: Bool
}

charToTile :: Char -> Tile
charToTile ' ' = Void
charToTile '.' = Path
charToTile '#' = Wall
charToTile _   = Void

parseMaze :: String -> Maze
parseMaze s = Maze{
        tiles=Data.Map.map charToTile q, 
        portals=fromListWith (++) . map (\(p1, p2) -> (p1, [p2])) . concat $ c, 
        start=start, 
        end=end,
        height=h,
        width=w
    }
    where
        getPartialPortals :: Map Position Char -> Position -> [(String, [Position])]
        getPartialPortals m p
            | isLetter s = (h East) ++ (h South)
            | otherwise = []
            where
                s = findWithDefault ' ' p m
        
                h :: Direction -> [(String, [Position])]
                h d = if isLetter v then [([s, v], x)] else []
                    where
                        n = addTuples p (movementTuple d)
                        v = findWithDefault ' ' n m 
                        p1 = (map ((addTuples n) . movementTuple) allDirections)
                        p2 = (map ((addTuples p) . movementTuple) allDirections)
                        x = filter (\p -> findWithDefault ' ' p m == '.') (p1 ++ p2)
                        
        l = lines s
        h = length l
        w = length (head l)

        q = fromList [((x, y), (l !! y) !! x) | x <- [0..w-1], y <- [0..h-1]]
        z = fromListWith (++) . concat . map (getPartialPortals q) . keys $ q

        [start] = z ! ("AA")
        [end] = z ! ("ZZ")

        c = [[(p1, p2) | p1 <- l, p2 <- l, p1 /= p2] | (k, l) <- assocs z, k /= "ZZ", k /= "AA"]

findShortestPath :: Maze -> MazeState -> Maybe [Move]
findShortestPath m s@MazeState{position=p@(px, py), layer=l, path=h, visited=v, recurse=r}
    | l == 0 && p == end m = Just h
    | l >= 30 = Nothing
    | null nextResult = Nothing
    | otherwise = Just (argmin length nextResult)
    where
        outside = (px <= 4) || (py <= 4) || (px >= (width m) - 4) || (py >= (height m) - 4)
        layerShift = if r then (if outside then -1 else 1) else 0

        portalDest = [
                (x, l + layerShift, Portal) |
                x <- findWithDefault [] p (portals m)
            ]

        moveDest = [
                (x, l, Move) | 
                d <- allDirections, 
                let x = addTuples p (movementTuple d), 
                findWithDefault Void x (tiles m) == Path
            ]

        nextResult = [
                fromJust r | 
                z@(y, nl, mt) <- (portalDest ++ moveDest), 
                notMember z v, 
                nl >= 0,
                let r = findShortestPath m s{position=y, layer=nl, path=h ++ [z], visited=Data.Set.insert z v},
                isJust r
            ]

solveMaze :: Maze -> Bool -> Maybe [Move]
solveMaze m@Maze{start=p} r = findShortestPath m MazeState{position=p, layer=0, path=[], visited=empty, recurse=r}

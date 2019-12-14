module Common where

import Debug.Trace
    
import Data.List.Split
import Data.List
import Data.Map (Map, fromList, findWithDefault, keys, filter, insert, toList)
import Intcode

data Tile = Empty | Wall | Block | Paddle | Ball deriving (Eq)

data Cabinet = Cabinet {
    state :: IntcodeState,
    display :: Map (Int, Int) Int
}

instance Show Tile where
    show t = [tileToChar t]

tileToChar :: Tile -> Char
tileToChar Empty  = ' '
tileToChar Wall   = '%'
tileToChar Block  = '#'
tileToChar Paddle = '='
tileToChar Ball   = 'o'

intToTile :: Int -> Tile
intToTile 0 = Empty
intToTile 1 = Wall
intToTile 2 = Block
intToTile 3 = Paddle
intToTile 4 = Ball
intToTile x = error ("Invalid tile type " ++ (show x))

createMap :: [Int] -> Map (Int, Int) Int
createMap x = fromList [((a, b), c) | (a:b:c:[]) <- chunksOf 3 x]

showMap :: Map (Int, Int) Int -> String
showMap t = (intercalate "\n" lines) ++ "\nScore: " ++ (show score)
    where
        k = keys t
        score = findWithDefault 0 (-1, 0) t
        maxx = maximum ([0] ++ [x | (x, _) <- k])
        maxy = maximum ([0] ++ [y | (_, y) <- k])
        lines = [[tileToChar (intToTile (findWithDefault 0 (x, y) t)) | x <- [0..maxx]] | y <- [0..maxy]]
        
runCabinet :: Cabinet -> Cabinet
runCabinet c@Cabinet{state=s@IntcodeState{halt=h}, display=m}
    | h = c
    | otherwise = runCabinet newCabinet
    where
        (q, o) = takeAllOutput (runProgramUntilNeedInput s)
                    
        newMap = foldl (\m (p, v) -> Data.Map.insert p v m) m [((a, b), c) | (a:b:c:[]) <- chunksOf 3 o]        
        mm = toList newMap
        
        Just ((px, _), _) = find (\(_, t) -> t == 3) mm 
        Just ((bx, _), _) = find (\(_, t) -> t == 4) mm 
            
        d = signum (bx - px)
        
        newCabinet = c{state=addInput q [d], display=newMap}
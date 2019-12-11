module Paintbot where

import Intcode
import Data.Map (Map, empty, fromList, insert, findWithDefault, keys)
import Data.List

type Position = (Int, Int)
data Tile = White | Black deriving (Show)
data Direction = North | South | West | East deriving (Show)
data Turn = TLeft | TRight

data Paintbot = Paintbot {
    tiles :: Map Position Tile,
    position :: Position,
    direction :: Direction,
    driver :: IntcodeState
}

intToColour :: Int -> Tile
intToColour 0 = Black
intToColour 1 = White

colourToInt :: Tile -> Int
colourToInt Black = 0
colourToInt White = 1

colourToChar :: Tile -> Char
colourToChar Black = '.'
colourToChar White = '#'

turn :: Int -> Turn
turn 0 = TLeft
turn 1 = TRight

changeDir :: Direction -> Turn -> Direction
changeDir North TLeft  = West
changeDir West  TLeft  = South
changeDir South TLeft  = East
changeDir East  TLeft  = North
changeDir North TRight = East
changeDir West  TRight = North
changeDir South TRight = West
changeDir East  TRight = South

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) North = (x, y + 1)
move (x, y) West  = (x - 1, y)
move (x, y) South = (x, y - 1)
move (x, y) East  = (x + 1, y)

intcodeStop :: IntcodeState -> Bool
intcodeStop (IntcodeState{output=o, halt=h}) = h || length o >= 2

newPaintbot :: [Int] -> [(Position, Tile)] -> Paintbot
newPaintbot x y = Paintbot{tiles=fromList y, position=(0, 0), direction=North, driver=fromTapeList x}

runPaintbot :: Paintbot -> Paintbot
runPaintbot b@Paintbot{tiles=t, position=p, direction=d, driver=q@IntcodeState{input=qi, halt=h}}
    | h = b
    | otherwise = runPaintbot newBot
    where
        currentTile = colourToInt (findWithDefault Black p t)
        
        (bt, o) = takeOutput (runProgramUntil intcodeStop (addInput q [currentTile])) 2
        
        newColor = if length o == 2 then intToColour (o !! 0) else Black
        newTurn = if length o == 2 then turn (o !! 1) else TLeft
        
        newDirection = changeDir d newTurn
        newPosition = move p newDirection
    
        newBot = Paintbot{
            tiles=Data.Map.insert p newColor t, 
            direction=newDirection, 
            position=newPosition, 
            driver=bt
        }
        
showTiles :: Map Position Tile -> String
showTiles t = intercalate "\n" lines
    where
        k = keys t
        minx = minimum [x | (x, _) <- k]
        maxx = maximum [x | (x, _) <- k]
        miny = minimum [y | (_, y) <- k]
        maxy = maximum [y | (_, y) <- k]
        lines = [[colourToChar (findWithDefault Black (x, y) t) | x <- [minx..maxx]] | y <- reverse [miny..maxy]]
        
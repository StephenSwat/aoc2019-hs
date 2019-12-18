module Cardinal where

type Position = (Int, Int)

data Direction = North | West | South | East deriving (Show)
data TurnDir = Leftward | Rightward deriving (Eq, Show)

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

movementTuple :: Direction -> (Int, Int)
movementTuple North = (0, -1)
movementTuple South = (0, 1)
movementTuple East  = (1, 0)
movementTuple West  = (-1, 0)

applyTurn :: TurnDir -> Direction -> Direction
applyTurn Leftward  North = West
applyTurn Leftward  West  = South
applyTurn Leftward  South = East
applyTurn Leftward  East  = North
applyTurn Rightward North = East
applyTurn Rightward West  = North
applyTurn Rightward South = West
applyTurn Rightward East  = South

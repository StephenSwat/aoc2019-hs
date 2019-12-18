module Common where

import Data.List
import Data.Set (Set, fromList, delete, member)
import Control.Monad
import Intcode
import Cardinal

data Movement = Turn TurnDir | Move Int deriving (Eq, Show)

data RobotState = RobotState {
    tiles :: Set Position,
    position :: Position,
    direction :: Direction,
    moveStack :: [Movement],
    functions :: [[Movement]],
    funStack :: [Int],
    unvisited :: Set Position
} deriving (Show)

charToDirection :: Char -> Direction
charToDirection '^' = North
charToDirection '<' = West
charToDirection 'v' = South
charToDirection '>' = East

moveToAscii :: Movement -> String
moveToAscii (Turn Leftward) = "L"
moveToAscii (Turn Rightward) = "R"
moveToAscii (Move x) = show x

movesToAscii :: [Movement] -> String
movesToAscii x = (intercalate "," (map moveToAscii x)) ++ "\n"

stateFromMap :: String -> RobotState
stateFromMap m = RobotState{
        tiles=fromList (s:allTiles),
        position=s,
        direction=charToDirection (getPosition s),
        moveStack=[],
        functions=[],
        funStack=[],
        unvisited=fromList allTiles
    }
    where
        l = filter ((> 0) . length) . lines $ m
        p = [(x, y) | y <- [0..(length l) - 1], x <- [0..(length (head l)) - 1]]
        allTiles = [q | q <- p, isScaffolding q]

        getPosition :: (Int, Int) -> Char
        getPosition (x, y) = ((l !! y) !! x)

        isScaffolding :: (Int, Int) -> Bool
        isScaffolding p = (getPosition p) == '#'

        isStart :: (Int, Int) -> Bool
        isStart p = elem (getPosition p) ['^', '<', 'v', '>']

        Just s = find isStart p

reduceState :: RobotState -> RobotState
reduceState s@RobotState{funStack=fs, functions=fun, moveStack=ms} = ns
    where
        q = findIndex (== ms) fun
        
        ni = case q of
            Just x  -> x
            Nothing -> length fun
        
        nf = case q of
            Just _  -> fun
            Nothing -> fun ++ [ms]

        ns = s{moveStack=[], funStack=fs ++ [ni], functions=nf}

squashMoves :: [Movement] -> [Movement]
squashMoves ([]) = []
squashMoves (Move x:Move y:n) = squashMoves ((Move (x + y)):n)
squashMoves (x:n) = x:(squashMoves n)

addMovement :: RobotState -> Movement -> RobotState
addMovement s@RobotState{moveStack=m, direction=od} n@(Turn d) = s{moveStack=squashMoves (m ++ [n]), direction=applyTurn d od}
addMovement s@RobotState{moveStack=m, direction=od, position=op, unvisited=ou} n = s{moveStack=squashMoves (m ++ [n]), position=np, unvisited=Data.Set.delete np ou}
    where
        np = addTuples op (movementTuple od)

maxLength = 10

isLegalState :: RobotState -> Bool
isLegalState RobotState{tiles=t, position=pos, functions=fun, moveStack=mov, funStack=fs} = vm && vc && vs && vp && vs
    where
        vm = length mov <= maxLength
        vc = length fun <= 3
        vs = length fs <= maxLength
        vf = all ((<= maxLength) . length) fun
        vp = member pos t

solve :: RobotState -> Maybe RobotState
solve s@RobotState{unvisited=unv, moveStack=mov}
    | not (isLegalState s) = Nothing 
    | null unv && null mov = Just s
    | null unv = solve (reduceState s)
    | otherwise = msum [solve (f x) | x <- nextStates, f <- [id, reduceState]]
    where
        nextStates = [
                addMovement s (Move 1),
                addMovement (addMovement s (Turn Leftward)) (Move 1),
                addMovement (addMovement s (Turn Rightward)) (Move 1)
            ]

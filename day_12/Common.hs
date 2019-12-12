module Common where

import Data.List.Split

type Pos = (Int, Int, Int)
type Vel = (Int, Int, Int)

data Body = Body {
    pos :: Pos,
    vel :: Vel
} deriving (Show)

getDelta :: Ordering -> Int
getDelta LT = -1
getDelta EQ = 0
getDelta GT = 1

sumAbsVector :: (Int, Int, Int) -> Int
sumAbsVector (a, b, c) = (abs a) + (abs b) + (abs c)

addVector :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addVector (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)

sumVelocities :: [Vel] -> Vel
sumVelocities = foldl addVector (0, 0, 0)

parseLine :: String -> Body
parseLine s = Body{pos=(x, y, z), vel=(0, 0, 0)}
    where
        [x, y, z] = ((map (read :: String -> Int)) . (map (drop 2)) . (splitOn ", ") . init . tail) s

parseString :: String -> [Body]
parseString = (map parseLine) . lines

getGravity :: Body -> Body -> Vel
getGravity Body{pos=(ax, ay, az)} Body{pos=(bx, by, bz)} = (
        getDelta (compare bx ax), 
        getDelta (compare by ay), 
        getDelta (compare bz az)
    )

applyVelocity :: Body -> Body
applyVelocity b@Body{pos=p, vel=v} = b{pos=addVector p v}

applyGravity :: Body -> [Body] -> Body
applyGravity t@Body{vel=v} r = t{vel=addVector v (sumVelocities vs)}
    where 
        vs = [getGravity t x | x <- r]

timeStep :: [Body] -> [Body]
timeStep l = ((map applyVelocity) . (map (\x -> applyGravity x l))) l

bodyEnergy :: Body -> Int
bodyEnergy Body{pos=p, vel=v} = (sumAbsVector p) * (sumAbsVector v)

systemEnergy :: [Body] -> Int
systemEnergy = (foldl (+) 0) . (map bodyEnergy)

horizontal :: [Body] -> ([Int], [Int], [Int])
horizontal l = foldl (\(lx, ly, lz) Body{pos=(tx, ty, tz), vel=(vx, vy, vz)} -> (lx ++ [tx, vx], ly ++ [ty, vy], lz ++ [tz, vz])) ([], [], []) l

module Common where

import Data.List.Split

type Vec3 t = (t, t, t)

data Body = Body {
    pos :: Vec3 Int,
    vel :: Vec3 Int
} deriving (Show)


-- Some operations that work on Vec3 objects...

monop :: (Num x, Num y) => (x -> y) -> Vec3 x -> Vec3 y
monop f (x, y, z) = (f x, f y, f z)

binop :: (Num x, Num y, Num z) => (x -> y -> z) -> Vec3 x -> Vec3 y -> Vec3 z
binop f (ax, ay, az) (bx, by, bz) = (f ax bx, f ay by, f az bz)


-- These functions parse the problem input.

parseLine :: String -> Body
parseLine s = Body{pos=(x, y, z), vel=(0, 0, 0)}
    where
        q = (map (read :: String -> Int)) . (map (drop 2)) . (splitOn ", ") . init . tail
        [x, y, z] = q s

parseString :: String -> [Body]
parseString = (map parseLine) . lines


-- These functions define the logic of doing a discrete time step.

stepBody :: Body -> [Body] -> Body
stepBody t@Body{pos=tp, vel=tv} r = t{vel=nv, pos=binop (+) tp nv}
    where 
        nv = foldl (binop (+)) tv [monop signum (binop (-) x tp) | Body{pos=x} <- r]

timeStep :: [Body] -> [Body]
timeStep l = (map (\x -> stepBody x l)) l


-- Functions to calculate the energy in a system, and in individual bodies.

bodyEnergy :: Body -> Int
bodyEnergy Body{pos=p, vel=v} = (sumAbsVector p) * (sumAbsVector v)
    where
        sumAbsVector (a, b, c) = (abs a) + (abs b) + (abs c)

systemEnergy :: [Body] -> Int
systemEnergy = sum . (map bodyEnergy)


-- Eww.

horizontal :: [Body] -> ([Int], [Int], [Int])
horizontal l = foldl (\(lx, ly, lz) Body{pos=(tx, ty, tz), vel=(vx, vy, vz)} -> (lx ++ [tx, vx], ly ++ [ty, vy], lz ++ [tz, vz])) ([], [], []) l

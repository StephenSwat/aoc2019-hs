module Dijkstra where

import Data.List.Extras.Argmax
import Data.Map (Map, insert, toList, singleton, empty, findWithDefault, (!), keys, filterWithKey, assocs, fromList)
import Data.Set (Set, insert, singleton, delete, empty, toList, notMember, isSubsetOf, isProperSubsetOf, unions)

import Common

type KeyNode = (Map Int Tile, Set Char)
data DijkstraState = DijkstraState {
    distance :: Map KeyNode Int,
    previous :: Map KeyNode KeyNode,
    queue :: Set KeyNode
} deriving (Show)

updateKeynode :: KeyNode -> (Int, Tile, Int) -> KeyNode
updateKeynode (cn, ck) (i, t, l) = (Data.Map.insert i t cn, nk)
    where
        nk = case t of
            Key k -> Data.Set.insert k ck
            _     -> ck

updateState :: KeyNode -> DijkstraState -> (Int, Tile, Int) -> DijkstraState
updateState c@(_, ck) s@DijkstraState{distance=d, previous=p, queue=q} m@(i, t, l)
    | dc && uk && sc = s{
            distance=Data.Map.insert tt nd d, 
            previous=Data.Map.insert tt c p, 
            queue=Data.Set.insert tt q
        }
    | otherwise = s
    where
        tt@(tn, _) = updateKeynode c m
        nd = (d ! c) + l
        dc = nd < (findWithDefault 1000000000 tt d)
        uk = ck `isProperSubsetOf` (snd tt)
        sc = not . any (\((n, k), l) -> tn == n && l <= nd && (snd tt) `isSubsetOf` k) . Data.Map.toList $ d

getNeighbours :: Maze -> KeyNode -> [(Int, Tile, Int)]
getNeighbours m c@(cn, ck) = [
        (i, x, y) |
        (i, c) <- assocs cn,
        (x, y, k) <- (m ! c),
        k `isSubsetOf` ck
    ]

dijkstraHelper :: Maze -> DijkstraState -> DijkstraState
dijkstraHelper m s@DijkstraState{distance=d, previous=p, queue=q}
    | null q = s
    | otherwise = dijkstraHelper m newState{queue=Data.Set.delete c (queue newState)}
    where
        c = argmin (\x -> findWithDefault 1000000000 x d) . Data.Set.toList $ q
        newState = foldl (updateState c) s . getNeighbours m $ c

dijkstra :: Maze -> KeyNode -> (KeyNode, Int)
dijkstra m s = r
    where
        fs@DijkstraState{distance=d} = dijkstraHelper m DijkstraState{
            distance=Data.Map.singleton s 0,
            previous=Data.Map.empty,
            queue=Data.Set.singleton s
        }

        maxKeys = unions . map snd . keys $ d
        q = filterWithKey (\(_, k) _ -> k == maxKeys) d
        r = argmin snd . Data.Map.toList $ q

isOrigin :: Tile -> Bool
isOrigin (Origin _) = True
isOrigin _ = False

initialKeynode :: Maze -> KeyNode
initialKeynode m = (fromList [
        (k, t) |
        t <- keys m,
        isOrigin t,
        let Origin k = t
    ], Data.Set.empty)

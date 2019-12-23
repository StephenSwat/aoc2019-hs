module Common where

import Debug.Trace

import Data.Maybe
import Data.Map (Map, fromListWith, findWithDefault, empty, insert, insertWith, unionWith, member, (!))
import Intcode

type Packet = (Int, Int)

data Network = Network {
    nodes :: [IntcodeState],
    nat :: Maybe Packet,
    natHistory :: [Packet],
    time :: Int,
    idle :: Int
}

initNetwork :: IntcodeState -> Int -> Network
initNetwork s n = Network {
        nodes=[addInput s [i] | i <- [0..n-1]],
        nat=Nothing,
        natHistory=[],
        time=0,
        idle=0
    }

packetToInput :: Packet -> [Int]
packetToInput (x, y) = [x, y]

stepNetwork :: Network -> Network
stepNetwork n = n{
        nodes=map runProgramUntilNeedInput ns3, 
        nat=if member 255 om then Just (head (om ! 255)) else nat n,
        natHistory=natHead ++ (natHistory n),
        time=nt,
        idle=if np == 0 then (idle n) else nt
    }
    where
        (ns1, o) = unzip [if length (output i) >= 3 then takeOutput i 3 else (i, []) | i <- (nodes n)]
        
        isIdle = (isJust . nat $ n) && (time n - idle n) > 10
        natHead = if isIdle then [fromJust . nat $ n] else []
        
        qz = ((0, natHead):[(d, [(x, y)]) | [d, x, y] <- filter ((== 3) . length) o])
        om = fromListWith (++) qz
        np = sum . map (\(_, l) -> length l) $ qz

        ns2 = [
                addInput q ol |
                (q, i) <- zip ns1 [0..],
                let ol = foldl (\a x -> a ++ (packetToInput x)) [] (findWithDefault [] i om)
            ]

        ns3 = [if nextOpcode s == Write && null (input s) then addInput s [-1] else s | s <- ns2]
        nt = succ . time $ n

module Common where

import Data.List
import Data.Maybe

data Shuffle = Reverse | Cut Integer | Deal Integer deriving (Show, Eq)

parseShuffle :: String -> Shuffle
parseShuffle s 
    | isPrefixOf "cut "                 s = Cut (read . fromJust . stripPrefix "cut " $ s)
    | isPrefixOf "deal with increment " s = Deal (read . fromJust . stripPrefix "deal with increment " $ s)
    | isPrefixOf "deal into new stack"  s = Reverse
    | otherwise = error "Invalid shuffle!"

parseShuffles :: String -> [Shuffle]
parseShuffles = map parseShuffle . lines

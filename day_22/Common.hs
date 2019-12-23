module Common where

import Data.Maybe
import Data.Map ((!), fromList)
import Data.Modular
import Data.List
import Data.List.Extras.Argmax
import Data.Semigroup
import Math.NumberTheory.Euclidean

data Shuffle = Reverse | Cut Integer | Deal Integer deriving (Show, Eq)

data BinopType = Add | Mul deriving (Show, Eq)
data Arithmetic = Binop BinopType Arithmetic Arithmetic | Mod Integer Arithmetic | Constant Integer | Variable deriving (Show, Eq)

instance Semigroup Arithmetic where
    (<>) (Binop f l r) n = simplify (Binop f (l <> n) (r <> n))
    (<>) (Mod m c) (Mod _ v) = simplify (Mod m (c <> v))
    (<>) (Mod m c) n = simplify (Mod m (c <> n))
    (<>) (Variable) n = simplify n
    (<>) o _ = o

(<@>) :: Arithmetic -> Integer -> Integer
(<@>) (Binop q l r) n = f (l <@> n) (r <@> n)
    where
        f = case q of
            Add -> (+)
            Mul -> (*)
(<@>) (Mod m t) n = (t <@> n) `mod` m
(<@>) (Constant n) _ = n 
(<@>) (Variable) n = n 

shuffleNode :: Integer -> Shuffle -> Arithmetic
shuffleNode o (Reverse) = Binop Add (Constant (o - 1)) (Binop Mul (Constant (-1)) Variable)
shuffleNode o (Cut c)
    | c < 0 = shuffleNode o (Cut (o + c))
    | otherwise = Binop Add (Constant c) (Variable)
shuffleNode o (Deal c) = Binop Mul (Constant (mod m o)) (Variable) where (_, m, _) = extendedGCD c o

simplify :: Arithmetic -> Arithmetic
simplify (Binop Add (Constant l) (Constant r)) = Constant (l + r)
simplify (Binop Mul (Constant l) (Constant r)) = Constant (l * r)
simplify (Binop Add l r@(Binop Add rl rr))     = simplify (Binop Add (simplify (Binop Add l rl)) (simplify rr))
simplify (Binop Mul l r@(Binop Add rl rr))     = simplify (Binop Add (simplify (Binop Mul l rl)) (simplify (Binop Mul l rr)))
simplify (Binop Mul l r@(Binop Mul rl rr))     = simplify (Binop Mul (simplify (Binop Mul l rl)) (simplify rr))
simplify (Binop f l r)                         = n
    where
        sl = simplify l
        sr = simplify r
        n = if (sl /= l) || (sr /= r) then simplify (Binop f sl sr) else (Binop f l r)
simplify (Mod m c)                             = Mod m (modTree m . simplify $ c)
simplify e = e

modTree :: Integer -> Arithmetic -> Arithmetic
modTree n (Binop f l r) = Binop f (modTree n l) (modTree n r)
modTree n (Mod m c) = Mod m (modTree n c )
modTree n (Constant m) = Constant (m `mod` n)
modTree _ x = x

parseShuffle :: String -> Shuffle
parseShuffle s 
    | isPrefixOf "cut "                 s = Cut (read . fromJust . stripPrefix "cut " $ s)
    | isPrefixOf "deal with increment " s = Deal (read . fromJust . stripPrefix "deal with increment " $ s)
    | isPrefixOf "deal into new stack"  s = Reverse
    | otherwise = error "Invalid shuffle!"

parseShuffles :: String -> [Shuffle]
parseShuffles = Data.List.map parseShuffle . lines

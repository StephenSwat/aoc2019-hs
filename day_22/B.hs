import System.Environment
import Data.Map ((!), fromList)
import Data.List
import Data.List.Extras.Argmax
import Math.NumberTheory.Euclidean
import Common

data BinopType = Add | Mul | Sub deriving (Show, Eq)
data Arithetic = Binop BinopType Arithetic Arithetic | Mod Integer Arithetic | Constant Integer | Variable deriving (Show, Eq)

evaluate :: Arithetic -> Integer -> Integer
evaluate (Binop q l r) n = f (evaluate l n) (evaluate r n)
    where
        f = case q of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
evaluate (Mod m t) n = (evaluate t n) `mod` m
evaluate (Constant n) _ = n 
evaluate (Variable) n = n 

shuffleNode :: Integer -> Shuffle -> Arithetic
shuffleNode o (Reverse) = Binop Sub (Constant (o - 1)) (Variable)
shuffleNode o (Cut c)
    | c < 0 = shuffleNode o (Cut (o + c))
    | otherwise = Binop Add (Constant c) (Variable)
shuffleNode o (Deal c) = Binop Mul (Constant i) (Variable)
    where
        (_, m, _) = extendedGCD c o
        i = mod m o

composeNode :: Arithetic -> Arithetic -> Arithetic
composeNode (Binop f l r) n = Binop f (composeNode l n) (composeNode r n)
composeNode (Variable) n = n
composeNode o _ = o

shufflesToArithmetic :: Integer -> [Shuffle] -> Arithetic
shufflesToArithmetic o ([]) = error "Undefined for 0 shuffles"
shufflesToArithmetic o (x:[]) = shuffleNode o x
shufflesToArithmetic o (x:xs) = composeNode (shuffleNode o x) (shufflesToArithmetic o xs)

depth :: Arithetic -> Int
depth (Constant _) = 0
depth (Variable) = 0
depth (Binop _ l r) = (max (depth l) (depth r)) + 1

simplify :: Arithetic -> Arithetic
simplify e@(Binop Sub l r)                       = simplify (Binop Add l (Binop Mul (Constant (-1)) r))
simplify e@(Binop Add (Constant l) (Constant r)) = Constant (l + r)
simplify e@(Binop Mul (Constant l) (Constant r)) = Constant (l * r)
simplify e@(Binop Add l r@(Binop Add rl rr))     = simplify (Binop Add (simplify (Binop Add l rl)) (simplify rr))
simplify e@(Binop Mul l r@(Binop Add rl rr))     = simplify (Binop Add (simplify (Binop Mul l rl)) (simplify (Binop Mul l rr)))
simplify e@(Binop Mul l r@(Binop Mul rl rr))     = simplify (Binop Mul (simplify (Binop Mul l rl)) (simplify rr))
simplify e@(Binop f l r)                         = n
    where
        sl = simplify l
        sr = simplify r
        n = if (sl /= l) || (sr /= r) then simplify (Binop f sl sr) else e
simplify e@(Mod m c)                             = Mod m (simplify c)
simplify e = e

modTree :: Integer -> Arithetic -> Arithetic
modTree n (Binop f l r) = Binop f (modTree n l) (modTree n r)
modTree n (Mod m c) = Mod m (modTree n c )
modTree n (Constant m) = Constant (m `mod` n)
modTree _ x = x

composeNTimesMod :: Integer -> Integer -> Arithetic -> Arithetic
composeNTimesMod n m p
    | n == 1 = p
    | c == 0 = modTree m . simplify $ f
    | c == 1 = modTree m . simplify . composeNode p $ f
    where
        (r, c) = n `divMod` 2
        z = modTree m . simplify . composeNode p $ p
        f = modTree m . simplify . composeNTimesMod r m $ z

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        c = 119315717514047
        t = 101741582076661

        i = parseShuffles s;
        b = shufflesToArithmetic c i

        q = Mod c (composeNTimesMod t c b)
    in do {
        print (evaluate q 2020);
    }
}

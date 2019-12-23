import Data.Maybe
import Data.List
import Data.List.Split
import Data.Map (member, (!))
import System.Environment
import Intcode
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        icm = initIntcode (map (read :: String -> Int) . splitOn "," $ s) []
        network = initNetwork icm 50

        states = iterate stepNetwork network
        Just Network{nat=n} = find (\Network{nat=n} -> isJust n) states
    in do {
        print (fromJust $ n)
    }
}

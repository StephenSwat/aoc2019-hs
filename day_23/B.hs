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
        Just Network{natHistory=n} = find (\m -> any ((> 1) . length) . groupBy (==) . map snd . natHistory $ m) states
    in do {
        print (snd . head $ n);
    }
}

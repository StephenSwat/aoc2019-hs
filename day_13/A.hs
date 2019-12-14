import Data.List.Split
import Data.Map (filter)
import System.Environment
import Intcode
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        ic = initIntcode (map (read :: String -> Int) . splitOn "," $ s) []
        IntcodeState{output=o} = runProgram ic
        m = createMap o
    in
    print . length . Data.Map.filter ((== Block) . intToTile) $ m;
}

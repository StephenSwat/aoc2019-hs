import Data.List.Split
import Data.List
import System.Environment
import Intcode

main = do {
    [f, h] <- getArgs;
    s <- readFile f;
    let 
        initial = initIntcode (map (read :: String -> Int) . splitOn "," $ s) ([read h :: Int])
        IntcodeState{tape=t, output=o} = runProgram initial
    in
    print o;
}

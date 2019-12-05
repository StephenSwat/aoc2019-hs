import Data.List.Split
import Data.List
import System.Environment
import Common

main = do {
    [f, h] <- getArgs;
    s <- readFile f;
    let 
        tape = map (read :: String -> Int) . splitOn "," $ s
        input = [read h :: Int]
        state = State{tape=tape, pc=0, input=input, output=[]}
        State {tape=t, output=o} = runProgram state
    in
    print o;
}

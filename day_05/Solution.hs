import Data.List.Split
import Data.List
import System.Environment
import Common

main = do {
    [f, h] <- getArgs;
    s <- readFile f;
    let 
        State{tape=t, output=o} = runProgram State{
            tape=map (read :: String -> Int) . splitOn "," $ s, 
            pc=0, 
            input=[read h :: Int], 
            output=[]
        }
    in
    print o;
}

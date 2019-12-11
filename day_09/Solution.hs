import Data.List.Split
import Data.List
import Data.IntMap (fromList)
import System.Environment
import Intcode

main = do {
    [f, h] <- getArgs;
    s <- readFile f;
    let 
        IntcodeState{tape=t, output=o} = runProgram IntcodeState{
            tape=fromList $ zip [0..] (map (read :: String -> Int) . splitOn "," $ s), 
            pc=0, 
            input=[read h :: Int], 
            output=[],
            base=0,
            halt=False
        }
    in
    print o;
}

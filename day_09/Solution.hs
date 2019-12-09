import Data.List.Split
import Data.List
import Data.IntMap (fromList)
import System.Environment
import Common

main = do {
    [f, h] <- getArgs;
    s <- readFile f;
    let 
        State{tape=t, output=o} = runProgram State{
            tape=fromList $ zip [0..] (map (read :: String -> Int) . splitOn "," $ s), 
            pc=0, 
            input=[read h :: Int], 
            output=[],
            base=0
        }
    in
    print o;
}

import Data.List.Split
import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let p = map (read :: String -> Int) . splitOn "," $ s in
    print $ [(a, b) | a <- [0..99], b <- [0..99], (runProgram . (\x -> replace x 2 b) . (\x -> replace x 1 a) $ p) !! 0 == 19690720];
}

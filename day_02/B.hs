import Data.List.Split
import System.Environment
import Common
import Intcode

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        p = map (read :: String -> Int) . splitOn "," $ s 
    in
    print ([
        (a, b) | 
        a <- [0..99], 
        b <- [0..99],
        let t = (\x -> replaceTape x 2 b) . (\x -> replaceTape x 1 a) $ p,
        let s = initIntcode t [],
        (== 19690720) . head . toTapeList . runProgram $ s
    ]);
}

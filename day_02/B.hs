import Data.List.Split
import System.Environment
import Control.Lens
import Control.Lens.Operators
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
        let t = (element 2 .~ b) . (element 1 .~ a) $ p,
        let s = initIntcode t [],
        (== 19690720) . head . toTapeList . runProgram $ s
    ]);
}

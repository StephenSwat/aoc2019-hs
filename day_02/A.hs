import Data.List.Split
import Data.List
import System.Environment
import Control.Lens
import Control.Lens.Operators
import Intcode

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let
        t = (element 2 .~ 2) . (element 1 .~ 12) . map (read :: String -> Int) . splitOn "," $ s
        i = initIntcode t []
    in
    print $ head . toTapeList . runProgram $ i;
}

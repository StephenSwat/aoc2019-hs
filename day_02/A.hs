import Data.List.Split
import Data.List
import System.Environment
import Common
import Intcode

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let
        t = (\x -> replaceTape x 2 2) . (\x -> replaceTape x 1 12) . map (read :: String -> Int) . splitOn "," $ s
        i = initIntcode t []
    in
    print $ (!! 0) . toTapeList . runProgram $ i;
}

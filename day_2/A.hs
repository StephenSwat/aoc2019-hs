import Data.List.Split
import Data.List
import System.Environment
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    print $ intercalate "," . map show . runProgram . (\x -> replace x 2 2) . (\x -> replace x 1 12) . map (read :: String -> Int) . splitOn "," $ s;
}

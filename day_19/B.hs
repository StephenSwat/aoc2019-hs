import Data.List.Split
import Data.List
import System.Environment
import Intcode
import Debug.Trace
import Common

findSolution :: IntcodeState -> (Int, Int) -> (Int, Int) -> (Int, Int)
findSolution icm (sx, sy) (px, py)
    | dx == 0 && dy == 0 = (px, py) 
    | otherwise = findSolution icm (sx, sy) (px + dx, py + dy)
    where
        mx = px + sx - 1
        my = py + sy - 1

        ylst = map (\x -> (head x, length x)) . group $ [showBeamChar q | y <- [py..my], let q = head . snd . takeAllOutput . runProgramUntilOutput . addInput icm $ [mx, y]]
        xlst = map (\x -> (head x, length x)) . group $ [showBeamChar q | x <- [px..mx], let q = head . snd . takeAllOutput . runProgramUntilOutput . addInput icm $ [x, my]]

        dy = case ylst of
            [('.', x), _] -> x
            _             -> 0
        dx = case xlst of
            [('.', x), _] -> x
            _             -> 0

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        icm = initIntcode (map (read :: String -> Int) . splitOn "," $ s) []
        q = findSolution icm (100, 100) (200, 260)
    in do {
        print q;
    }
}

import Data.List
import Data.List.Split
import Data.Char (chr, ord)
import Control.Lens
import Control.Lens.Operators
import System.Environment
import Intcode
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        tape = map (read :: String -> Int) . splitOn "," $ s
        i = initIntcode tape [];
        IntcodeState{output=o} = runProgram i;
        l = map chr o;

        Just RobotState{functions=[fa, fb, fc], funStack=fs} = solve . stateFromMap $ l

        as = map ord . (++ "\n") . intersperse ',' . map chr . map (+ 65) $ fs
        aa = map ord . movesToAscii $ fa
        ab = map ord . movesToAscii $ fb
        ac = map ord . movesToAscii $ fc

        ta = map ord ("n\n");

        ni = initIntcode ((element 0 .~ 2) tape) (as ++ aa ++ ab ++ ac ++ ta)
        IntcodeState{output=no} = runProgram ni;
    in do {
        putStrLn ("Fun calls : " ++ (show fs));
        putStrLn ("        = : " ++ (show as));
        putStrLn ("Function A: " ++ (show fa));
        putStrLn ("        = : " ++ (show aa));
        putStrLn ("Function B: " ++ (show fb));
        putStrLn ("        = : " ++ (show ab));
        putStrLn ("Function C: " ++ (show fc));
        putStrLn ("        = : " ++ (show ac));
        putStrLn ("Output: " ++ (show . last $ no));
    }
}

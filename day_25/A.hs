import Data.List
import Data.List.Split
import Data.Char (chr, ord)
import System.Environment
import Intcode

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        i = "\
            \west\n\
            \north\n\
            \east\n\
            \take space law space brochure\n\
            \west\n\
            \take space heater\n\
            \south\n\
            \take hologram\n\
            \east\n\
            \east\n\
            \east\n\
            \east\n\
            \take spool of cat6\n\
            \west\n\
            \west\n\
            \south\n\
            \east\n\
            \east\n\
            \east\n\
            \south\n\
        \"
        IntcodeState{output=o} = runProgramUntilNeedInput . initIntcode (map (read :: String -> Int) . splitOn "," $ s) . map ord $ i;
    in do {
        putStrLn . map chr $ o;
    }
}

import Data.List.Split
import Data.Char (ord, chr)
import System.Environment
import Intcode

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        sol = "\
            \OR A J\n\
            \AND B J\n\
            \AND C J\n\
            \NOT J J\n\
            \AND D J\n\
            \WALK\n"
        i = initIntcode (map (read :: String -> Int) . splitOn "," $ s) (map ord sol);
        IntcodeState{output=o} = runProgram i;
    in do {
        putStrLn (map chr . init $ o);
        print (last o)
    }
}

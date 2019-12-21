import Data.List.Split
import Data.Char (ord, chr)
import System.Environment
import Intcode

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        sol = "\
            \OR A T\n\
            \AND B T\n\
            \AND C T\n\
            \NOT T T\n\
            \AND D T\n\
            \OR F J\n\
            \OR I J\n\
            \AND E J\n\
            \OR H J\n\
            \AND T J\n\
            \RUN\n"
        i = initIntcode (map (read :: String -> Int) . splitOn "," $ s) (map ord sol);
        IntcodeState{output=o} = runProgram i;
    in do {
        putStrLn (map chr . init $ o);
        print (last o)
    }
}

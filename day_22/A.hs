import System.Environment
import Data.List
import Data.List.NonEmpty (fromList, map)
import Data.Map ((!), fromList)
import Data.Semigroup
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        c = 10007

        i = parseShuffles s;
        b = Mod c (sconcat . Data.List.NonEmpty.map (shuffleNode c) . Data.List.NonEmpty.fromList $ i)
    in do {
        print (find ((== 2019) . (b <@>)) [0..c-1]);
    }
}

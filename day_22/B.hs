import System.Environment
import Data.List.NonEmpty (fromList, map)
import Data.Semigroup
import Common

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        c = 119315717514047
        t = 101741582076661

        i = parseShuffles s;
        b = sconcat . fmap (shuffleNode c) . Data.List.NonEmpty.fromList $ i

        q = stimes t (Mod c b)
    in do {
        print (q <@> 2020);
    }
}

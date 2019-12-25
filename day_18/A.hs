import System.Environment

import Preprocess (stringToTiles, tilesToMaze)
import Dijkstra (initialKeynode, dijkstra)

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        m = tilesToMaze . stringToTiles $ s;
        i = initialKeynode m;
        q = dijkstra m i;
    in do {
        print q;
    }
}

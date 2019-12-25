import System.Environment

import Data.Map (union, fromList)
import Preprocess (stringToTiles, tilesToMaze)
import Dijkstra (initialKeynode, dijkstra)
import Common (Tiles, Tile(Origin, Wall), showTiles)
import Cardinal

updateTiles :: Position -> Tiles -> Tiles
updateTiles (x, y) t = union nt t
    where 
        nt = fromList [
                ((x - 1, y - 1), Origin 0),
                ((x    , y - 1), Wall),
                ((x + 1, y - 1), Origin 1),
                ((x - 1, y    ), Wall),
                ((x    , y    ), Wall),
                ((x + 1, y    ), Wall),
                ((x - 1, y + 1), Origin 2),
                ((x    , y + 1), Wall),
                ((x + 1, y + 1), Origin 3)
            ]

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        m = tilesToMaze . updateTiles (40, 40) . stringToTiles $ s;
        i = initialKeynode m;
        q = dijkstra m i;
    in do {
        print q;
    }
}

import Data.Map (fromList, mapWithKey, findWithDefault, filter, mapKeys, filterWithKey, keys, elems)
import Data.List
import System.Environment
import Cardinal
import Common

stepGrid :: RecursiveGrid -> RecursiveGrid
stepGrid g = fromList [
        (p, t) |
        x <- [0..4],
        y <- [0..4],
        l <- [lmin..lmax],
        let p = (l, x, y),
        let t = stepTile (numberOfNeighbours p) (findWithDefault Empty p g),
        t == Bug
    ]
    where
        layers = map (\(l, _, _) -> l) . keys $ g
        lmin = (minimum layers) - 1
        lmax = (maximum layers) + 1

        neighbours :: (Int, Int, Int) -> [(Int, Int, Int)]
        neighbours (l, 2, 2) = []

        neighbours (l, 2, 1) = [(l, 1, 1), (l, 2, 0), (l, 3, 1)] ++ [(l + 1, x, 0) | x <- [0..4]]
        neighbours (l, 2, 3) = [(l, 1, 3), (l, 2, 4), (l, 3, 3)] ++ [(l + 1, x, 4) | x <- [0..4]]
        neighbours (l, 1, 2) = [(l, 1, 1), (l, 1, 3), (l, 0, 2)] ++ [(l + 1, 0, y) | y <- [0..4]]
        neighbours (l, 3, 2) = [(l, 3, 1), (l, 3, 3), (l, 4, 2)] ++ [(l + 1, 4, y) | y <- [0..4]]

        neighbours (l, 0, 0) = [(l, 1, 0), (l, 0, 1), (l - 1, 2, 1), (l - 1, 1, 2)]
        neighbours (l, 4, 0) = [(l, 3, 0), (l, 4, 1), (l - 1, 2, 1), (l - 1, 3, 2)]
        neighbours (l, 0, 4) = [(l, 1, 4), (l, 0, 3), (l - 1, 1, 2), (l - 1, 2, 3)]
        neighbours (l, 4, 4) = [(l, 3, 4), (l, 4, 3), (l - 1, 3, 2), (l - 1, 2, 3)]

        neighbours (l, x, 0) = [(l - 1, 2, 1), (l, x - 1, 0), (l, x + 1, 0), (l, x, 1)]
        neighbours (l, x, 4) = [(l - 1, 2, 3), (l, x - 1, 4), (l, x + 1, 4), (l, x, 3)]
        neighbours (l, 0, y) = [(l - 1, 1, 2), (l, 0, y - 1), (l, 0, y + 1), (l, 1, y)]
        neighbours (l, 4, y) = [(l - 1, 3, 2), (l, 4, y - 1), (l, 4, y + 1), (l, 3, y)]

        neighbours (l, x, y) = [(l, x - 1, y), (l, x + 1, y), (l, x, y - 1), (l, x, y + 1)]

        numberOfNeighbours :: (Int, Int, Int) -> Int
        numberOfNeighbours = length . Data.List.filter (== Bug) . map (\x -> findWithDefault Empty x g) . neighbours

extractGrid :: Int -> RecursiveGrid -> Grid
extractGrid l = mapKeys (\(_, x, y) -> (x, y)) . filterWithKey (\(x, _, _) _ -> l == x)

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        m = toRecursive . gridFromString $ s;
        g = iterate stepGrid m;
        r = g !! 200
        c = length . Data.List.filter (== Bug) . elems $ r;
    in do {
        print c;
    }
}

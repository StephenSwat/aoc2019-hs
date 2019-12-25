import Data.Map (mapWithKey, findWithDefault)
import Data.Set (Set, empty, member, insert)
import Data.List
import System.Environment
import Cardinal
import Common

stepGrid :: Grid -> Grid
stepGrid z = mapWithKey (\p t -> stepTile (neighbours p) t) z
    where
        neighbours :: (Int, Int) -> Int
        neighbours p = length . Data.List.filter (\q -> Bug == findWithDefault Empty q z) . map (addTuples p . movementTuple) $ allDirections

firstDuplicate :: [Grid] -> Grid
firstDuplicate n = firstDuplicateHelper n empty
    where
        firstDuplicateHelper :: [Grid] -> Set Grid -> Grid
        firstDuplicateHelper (g:xg) s
            | member g s = g
            | otherwise = firstDuplicateHelper xg (Data.Set.insert g s)

biodiversity :: Grid -> Integer
biodiversity g = sum [2^(x + 5 * y) | x <- [0..4], y <- [0..4], Bug == findWithDefault Empty (x, y) g]

main = do {
    [f] <- getArgs;
    s <- readFile f;
    let 
        m = gridFromString s;
        q = iterate stepGrid m;
        r = firstDuplicate q;
    in do {
        putStrLn . showGrid $ r;
        print (biodiversity r);
    }
}

module Common where
    
import Data.Maybe
import Data.List
import Data.Tree
import Data.Map.Strict (Map, insertWith, empty, findWithDefault)
import Data.List.Split

parseString :: String -> [(String, String)]
parseString = (map ((\x -> (x !! 0, x !! 1)) . splitOn ")")) . lines

toTree :: [(String, String)] -> Tree String
toTree i = unfoldTree children "COM"
    where 
        mp = foldl (\m (x, y) -> insertWith (++) x [y] m) (empty :: Map String [String]) i
        children x = (x, findWithDefault [] x mp)
        
depthSum' :: Eq t => Tree t -> Integer -> Integer
depthSum' (Node _ []) n = n 
depthSum' (Node _ ch) n = n + sum ([depthSum' l (n + 1) | l <- ch])
        
depthSum :: Eq t => Tree t -> Integer
depthSum x = depthSum' x 0

maybeMin :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeMin (Nothing) (x) = x
maybeMin (x) (Nothing) = x
maybeMin (Just x) (Just y) = Just (min x y)

depth :: Eq t => Tree t -> t -> Maybe Integer
depth (Node x ch) n
    | x == n = Just 0
    | length ch == 0 = Nothing 
    | Just x <- md = Just (x + 1)
    | otherwise = Nothing
    where 
        md = foldl maybeMin Nothing [depth c n | c <- ch]
    
isAncestor :: Eq t => Tree t -> t -> Bool
isAncestor (Node x ch) n
    | x == n = True
    | otherwise = any (\x -> isAncestor x n) ch

deepestCommonAncestor :: Eq t => Tree t -> t -> t -> Tree t
deepestCommonAncestor x i j
    | Nothing <- c = x
    | Just y <- c = deepestCommonAncestor y i j
    where
        (Node _ ch) = x
        c = find (\x -> (isAncestor x i) && (isAncestor x j)) ch
        
shortestPath :: Eq t => Tree t -> t -> t -> Integer
shortestPath t i j = di + dj - 2
    where
        Just di = depth t i
        Just dj = depth t j
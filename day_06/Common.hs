module Common where
    
import Data.Maybe
import Data.List
import Data.Tree
import Data.Map.Strict (Map, insertWith, empty, findWithDefault)
import Data.List.Split

-- Given a string, containing lines in the form "str1)str2", produce a list of
-- tuples (str1, str2) for each line.
parseString :: String -> [(String, String)]
parseString = map (\x -> (x !! 0, x !! 1)) . map (splitOn ")") . lines

-- Given a list of tuples (str1, str2), create a tree where str2 is a child of
-- str1. We do this by first creating a mapping of parent nodes to child nodes,
-- and then we transform this into a tree.
toTree :: [(String, String)] -> Tree String
toTree i = unfoldTree children "COM"
    where 
        mp = foldl (\m (x, y) -> insertWith (++) x [y] m) empty i
        children x = (x, findWithDefault [] x mp)

-- Given a tree, get the sum of the depth of each node. This solves problem A
-- of finding the total number of direct and indirect orbits.
depthSum :: Eq t => Tree t -> Integer
depthSum x = depthSum' x 0
    where 
        depthSum' (Node _ []) n = n
        depthSum' (Node _ ch) n = n + sum [depthSum' l (n + 1) | l <- ch]

-- Helper function that takes two Maybe Integers and returns the smallest of
-- the two, discarding Nothings, and returning Nothing only if both inputs are
-- nothing.
maybeMin :: Maybe Integer -> Maybe Integer -> Maybe Integer
maybeMin (Nothing) (x) = x
maybeMin (x) (Nothing) = x
maybeMin (Just x) (Just y) = Just (min x y)

-- Given a tree and an element in that tree, return the depth of that element
-- in the tree. If the element is not in the tree, return Nothing.
depthTo :: Eq t => Tree t -> t -> Maybe Integer
depthTo (Node x ch) n
    | x == n = Just 0
    | Just x <- md = Just (x + 1)
    | otherwise = Nothing
    where 
        md = foldl maybeMin Nothing [depthTo c n | c <- ch]

-- Returns true if a tree contains a certain node. In other words, the head of
-- the (sub)tree is an ancestor of the given value.
isAncestor :: Eq t => Tree t -> t -> Bool
isAncestor (Node x ch) n
    | x == n = True
    | otherwise = any (\x -> isAncestor x n) ch

-- Given a tree and two nodes, return the subtree starting at the deepest 
-- common ancestor of those two nodes. This probably breaks if there is no
-- common ancestor. This returns the smallest possible sub-tree that contains
-- both nodes, and the shortest path must always pass through the root of this
-- tree.
deepestCommonAncestor :: Eq t => Tree t -> t -> t -> Tree t
deepestCommonAncestor x i j
    | Nothing <- c = x
    | Just y <- c = deepestCommonAncestor y i j
    where
        (Node _ ch) = x
        c = find (\x -> (isAncestor x i) && (isAncestor x j)) ch

-- Find the length of the shortest path between two nodes in a tree. First, we
-- find the deepest common ancestor. Then we find the depth of the two nodes
-- relative to the subtree. The distance is the sum of those depths.
shortestPath :: Eq t => Tree t -> t -> t -> Integer
shortestPath t i j = di + dj
    where
        t' = deepestCommonAncestor t i j
        Just di = depthTo t' i
        Just dj = depthTo t' j
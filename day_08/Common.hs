module Common where

type Layer = [Int]

-- Given a string, simply decode it into integers.
getImage :: String -> [Int]
getImage s = [read [x] | x <- s, x == '0' || x == '1' || x == '2']

-- Given a list of integers and an image size (h, w), return a list of image
-- layers, or give an error if the total length of the imput is not neatly
-- divided by the image size.
layers :: [Int] -> (Int, Int) -> [Layer]
layers img s@(h, w)
    | null n = [l]
    | length img >= layer_size = l : layers n s
    | otherwise = error ("Layer size " ++ (show (length img)) ++ " does not divide image size!")
    where 
        layer_size = h * w
        (l, n) = splitAt layer_size img

-- Given a list of layers, return the overlay of those image, starting at the
-- end of the list, going to the front, such that each layer overwrites the
-- previous layers if it has value 0 or 1, but keep the old value if the new
-- layer has a transparent pixel with value 2.
render :: [Layer] -> Layer
render x = foldr1 f x
    where
        f :: Layer -> Layer -> Layer
        f x y = map (\(a, b) -> if a /= 2 then a else b) (zip x y)

-- Given an image and a size (h, w), return a printable string that shows that
-- image.
showImage :: Layer -> (Int, Int) -> String
showImage [] _ = ""
showImage x s@(_, w) = (concat . map show $ l) ++ "\n" ++ (showImage n s)
    where
        (l, n) = splitAt w x

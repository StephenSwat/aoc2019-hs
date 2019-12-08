module Common where

type Layer = [Int]

-- Given a string, simply decode it into integers.
getImage :: String -> [Int]
getImage s = [(read :: String -> Int) [x] | x <- s, x == '0' || x == '1' || x == '2']

-- Given a list of integers and an image size (h, w), return a list of image
-- layers, or give an error if the total length of the imput is not neatly
-- divided by the image size.
layers :: [Int] -> (Int, Int) -> [Layer]
layers img s@(h, w)
    | length img == layer_size = [img]
    | length img >= layer_size = [take layer_size img] ++ layers (drop layer_size img) s
    | otherwise = error ("Layer size " ++ (show (length img)) ++ " does not divide image size!")
    where 
        layer_size = h * w

-- Given a list of layers, return the overlay of those image, starting at the
-- end of the list, going to the front, such that each layer overwrites the
-- previous layers if it has value 0 or 1, but keep the old value if the new
-- layer has a transparent pixel with value 2.
render :: [Layer] -> Layer
render x = foldr f [2 :: Int | _ <- head x] x
    where
        f :: Layer -> Layer -> Layer
        f x y = map (\(a, b) -> if a /= 2 then a else b) (zip x y)

-- Given an image and a size (h, w), return a printable string that shows that
-- image.
showImage :: Layer -> (Int, Int) -> String
showImage ([]) (_, _) = ""
showImage x s@(h, w) = (concat . map show $ (take w x)) ++ "\n" ++ (showImage (drop w x) s)

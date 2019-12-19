module Common where

import Intcode

showBeamChar :: Int -> Char
showBeamChar 0 = '.'
showBeamChar 1 = '#'

fetchSquare :: IntcodeState -> (Int, Int) -> (Int, Int) -> String
fetchSquare icm (x, y) (lx, ly) = concat [[showBeamChar q | x <- [x..x+lx-1], let q = head . snd . takeAllOutput . runProgramUntilOutput . addInput icm $ [x, y]] ++ "\n" | y <- [y..y+ly-1]]

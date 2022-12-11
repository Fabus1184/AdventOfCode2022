module Day6 (p1, p2) where

import Data.List.Extra (anySame, findIndex, tails)

f :: Eq a => Int -> [a] -> Int
f n =
    maybe (-1) (+ n)
        . findIndex (not . anySame)
        . map (take n)
        . tails

p1, p2 :: String -> Int
p1 = f 4
p2 = f 14
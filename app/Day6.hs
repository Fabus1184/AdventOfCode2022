module Day6 (p1, p2) where

import Data.List (findIndex, nub, tails)
import Data.Maybe (fromJust)

f :: Eq a => Int -> [a] -> String
f n =
    show
        . (+ n)
        . fromJust
        . findIndex (nub >>= (==))
        . map (take n)
        . tails

p1, p2 :: String -> String
p1 = f 4
p2 = f 14
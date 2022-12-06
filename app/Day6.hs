module Day6 (p1, p2) where

import Data.List (findIndex, tails)
import Data.List.Extra (anySame)
import Data.Maybe (fromJust)

f :: Eq a => Int -> [a] -> String
f n =
    show
        . (+ n)
        . fromJust
        . findIndex (not . anySame)
        . map (take n)
        . tails

p1, p2 :: String -> String
p1 = f 4
p2 = f 14
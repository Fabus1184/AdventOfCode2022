module Day1 (p1, p2) where

import Data.List.Extra (sortOn, splitOn)
import Data.Ord (Down (..))

readInput :: String -> [Int]
readInput =
    sortOn Down
        . map (sum . map read)
        . splitOn [[]]
        . lines

p1, p2 :: String -> Int
p1 = head . readInput
p2 = sum . take 3 . readInput

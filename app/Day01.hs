module Day01 (p1, p2) where

import Data.List (sortOn)
import Data.List.Extra (splitOn)
import Data.Ord (Down (..))

input :: IO [Int]
input =
    sortOn Down
        . map (sum . map read)
        . splitOn [[]]
        . lines
        <$> readFile "input01.txt"

p1 :: IO Int
p1 = head <$> input

p2 :: IO Int
p2 = sum . take 3 <$> input

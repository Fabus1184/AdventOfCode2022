module Day1 (p1, p2) where

import Data.List (sortOn)
import Data.List.Extra (splitOn)
import Data.Ord (Down (..))

input :: IO [Int]
input =
    sortOn Down
        . map (sum . map read)
        . splitOn [[]]
        . lines
        <$> readFile "input1.txt"

p1 :: IO String
p1 = show . head <$> input

p2 :: IO String
p2 = show . sum . take 3 <$> input

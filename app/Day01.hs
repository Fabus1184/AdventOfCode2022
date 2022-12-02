module Day01 (p1, p2) where

import Data.List (groupBy, sort)

input :: IO [Int]
input =
    reverse
        . sort
        . map (sum . map read)
        . filter (not . any null)
        . groupBy (\a b -> [] `notElem` [a, b])
        . lines
        <$> readFile "input01.txt"

p1 :: IO Int
p1 = head <$> input

p2 :: IO Int
p2 = sum . take 3 <$> input

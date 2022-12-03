module Day3 (p1, p2) where

import Data.Char (ord)
import Data.List (intersect)
import Data.List.Extra (chunksOf)

readPrio :: Char -> Int
readPrio c
    | c <= 'Z' = ord c - ord 'A' + 27
    | otherwise = succ $ ord c - ord 'a'

input :: IO [[Int]]
input = map (map readPrio) . lines <$> readFile "input3.txt"

p1 :: IO Int
p1 = sum . map (head . uncurry intersect . (\x -> splitAt (length x `div` 2) x)) <$> input

p2 :: IO Int
p2 = sum . map (head . foldl1 intersect) . chunksOf 3 <$> input

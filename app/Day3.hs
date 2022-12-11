module Day3 (p1, p2) where

import Data.Char (ord)
import Data.List.Extra (chunksOf, intersect)

readPrio :: Char -> Int
readPrio c
    | c <= 'Z' = ord c - ord 'A' + 27
    | otherwise = succ $ ord c - ord 'a'

readInput :: String -> [[Int]]
readInput = map (map readPrio) . lines

p1, p2 :: String -> Int
p1 = sum . map (head . uncurry intersect . ((`div` 2) . length >>= splitAt)) . readInput
p2 = sum . map (head . foldl1 intersect) . chunksOf 3 . readInput

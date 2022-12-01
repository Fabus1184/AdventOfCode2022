module Day01 (p1, p2) where

import Data.List (groupBy, sortOn)
import Data.Ord (Down (..))

input :: IO [[Int]]
input =
    sortOn (Down . sum)
        . map (map (read :: String -> Int))
        . filter (/= [""])
        . groupBy (\a b -> a /= "" && b /= "")
        . lines
        <$> readFile "input1.txt"

p1 :: IO Int
p1 = sum . head <$> input

p2 :: IO Int
p2 = sum . map sum . take 3 <$> input

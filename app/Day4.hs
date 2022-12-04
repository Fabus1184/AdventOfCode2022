module Day4 (p1, p2) where

import Data.List ((\\))
import Data.List.Extra (disjoint, splitOn)

readPair :: String -> ([Int], [Int])
readPair xs =
    let [[a, b], [c, d]] = map (splitOn "-") $ splitOn "," xs
     in (,) [read a .. read b] [read c .. read d]

input :: IO [([Int], [Int])]
input = map readPair . lines <$> readFile "input4.txt"

p1 :: IO Int
p1 = length . filter (\(a, b) -> any null [a \\ b, b \\ a]) <$> input

p2 :: IO Int
p2 = length . filter (not . uncurry disjoint) <$> input
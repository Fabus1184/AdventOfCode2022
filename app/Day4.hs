module Day4 (p1, p2) where

import Data.List ((\\))
import Data.List.Extra (disjoint, splitOn)

readPair :: String -> ([Int], [Int])
readPair xs =
    let [[a, b], [c, d]] = map (splitOn "-") $ splitOn "," xs
     in (,) [read a .. read b] [read c .. read d]

input :: IO [([Int], [Int])]
input = map readPair . lines <$> readFile "input4.txt"

p1 :: IO String
p1 = show . length . filter (\(a, b) -> any null [a \\ b, b \\ a]) <$> input

p2 :: IO String
p2 = show . length . filter (not . uncurry disjoint) <$> input
module Day4 (p1, p2) where

import Data.List.Extra (disjoint, splitOn, (\\))

readPair :: String -> ([Int], [Int])
readPair xs =
    let [[a, b], [c, d]] = map (map read . splitOn "-") $ splitOn "," xs
     in (,) [a .. b] [c .. d]

readInput :: String -> [([Int], [Int])]
readInput = map readPair . lines

p1, p2 :: String -> Int
p1 = length . filter (\(a, b) -> any null [a \\ b, b \\ a]) . readInput
p2 = length . filter (not . uncurry disjoint) . readInput

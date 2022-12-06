module Day4 (p1, p2) where

import Data.List.Extra (disjoint, splitOn, (\\))

readPair :: String -> ([Int], [Int])
readPair xs =
    let [[a, b], [c, d]] = map (map read . splitOn "-") $ splitOn "," xs
     in (,) [a .. b] [c .. d]

readInput :: String -> [([Int], [Int])]
readInput = map readPair . lines

p1, p2 :: String -> String
p1 = show . length . filter (\(a, b) -> any null [a \\ b, b \\ a]) . readInput
p2 = show . length . filter (not . uncurry disjoint) . readInput

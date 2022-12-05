module Day5 (p1, p2) where

import Control.Lens (Ixed (ix), (%~), (&))
import Data.Char (isAlpha, isNumber)
import Data.List (transpose)
import Data.List.Extra (splitOn)
import Lib (tmap2, tmap3, tup2, tup3)

readStack :: [String] -> [[Char]]
readStack = map (filter isAlpha) . transpose . map (\l -> map (l !!) [1, 5 .. 33]) . init

readMove :: String -> (Int, Int, Int)
readMove = tmap3 (id, pred, pred) . tup3 . map read . filter (all isNumber) . words

input :: IO ([[Char]], [(Int, Int, Int)])
input = tmap2 (readStack, map readMove) . tup2 . splitOn [[]] . lines <$> readFile "input5.txt"

run :: Bool -> [[Char]] -> (Int, Int, Int) -> [[Char]]
run p s (a, b, c) =
    let container = (if p then id else reverse) . take a . (!! b) $ s
     in s & (ix b %~ drop a) . (ix c %~ (container ++))

p1 :: IO String
p1 = show . map head . uncurry (foldl (run False)) <$> input

p2 :: IO String
p2 = show . map head . uncurry (foldl (run True)) <$> input
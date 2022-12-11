module Day5 (p1, p2) where

import Control.Lens (ix, (%~), (&))
import Data.Char (isAlpha, isNumber)
import Data.List.Extra (splitOn, transpose)
import Lib (if', tmap2, tmap3, tup2, tup3)

readStack :: [String] -> [[Char]]
readStack = map (filter isAlpha) . transpose . map (\l -> map (l !!) [1, 5 .. 33]) . init

readMove :: String -> (Int, Int, Int)
readMove = tmap3 (id, pred, pred) . tup3 . map read . filter (all isNumber) . words

readInput :: String -> ([[Char]], [(Int, Int, Int)])
readInput = tmap2 (readStack, map readMove) . tup2 . splitOn [[]] . lines

go :: Bool -> String -> String
go p = map head . uncurry (foldl run) . readInput
 where
  run s (a, b, c) =
    let container = if' p reverse $ take a (s !! b)
     in s & (ix b %~ drop a) . (ix c %~ (container ++))

p1, p2 :: String -> String
p1 = go True
p2 = go False
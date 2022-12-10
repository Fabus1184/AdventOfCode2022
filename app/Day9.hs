{-# LANGUAGE LambdaCase #-}

module Day9 (p1, p2) where

import Control.Arrow ((***))
import Data.Bifunctor (second)
import Data.List (nub)
import Data.Tuple.Extra (both, first)
import Lib ()

data Direction = U | D | L | R deriving (Eq, Show, Read)
type Position = (Int, Int)
type Rope = [Position]

directionalize :: Position -> Position -> [Direction]
directionalize (x1, y1) (x2, y2) = map snd $ filter fst [(x1 < x2, R), (x1 > x2, L), (y1 < y2, U), (y1 > y2, D)]

direction :: [Direction] -> Position -> Position
direction = foldl1 (.) . map (\case U -> second succ; D -> second pred; L -> first pred; R -> first succ)

move :: Direction -> Rope -> Rope
move dir ((x, y) : h' : tl) = let k = direction [dir] (x, y) in k : move' k (h' : tl)
  where
    move' :: Position -> Rope -> Rope
    move' _ [] = []
    move' to (k : ks)
        | uncurry (&&) (both (<= 1) $ abs (k - to)) = k : move' k ks
        | otherwise = let kk = direction (directionalize k to) k in kk : move' kk ks
move _ _ = error "move: invalid rope"

readInput :: String -> (Direction, Int)
readInput = (read *** read) . splitAt 1

f :: Int -> String -> String
f n =
    show
        . length
        . nub
        . fst
        . foldl (\(t, acc) m -> ((: t) . last) >>= (,) $ move m acc) ([], replicate n mempty)
        . concatMap (uncurry (flip replicate) . readInput)
        . lines

p1, p2 :: String -> String
p1 = f 2
p2 = f 10

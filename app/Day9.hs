{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Day9 (p1, p2) where

import Control.Arrow ((***))
import Control.Monad (void)
import Data.Bifunctor (bimap, first, second)
import Data.Foldable (foldlM)
import Data.Functor (($>))
import Data.List (nub)
import Debug.Trace (traceShowId)
import GHC.IO (unsafePerformIO)

data Direction = U | D | L | R deriving (Show)

type Tail = (Int, Int)
type Head = (Int, Int)

instance Read Direction where
    readsPrec :: Int -> ReadS Direction
    readsPrec _ = \case
        'U' : s -> [(U, s)]
        'D' : s -> [(D, s)]
        'L' : s -> [(L, s)]
        'R' : s -> [(R, s)]
        _ -> []

distance :: (Tail, Head) -> Float
distance ((x1, y1), (x2, y2)) = sqrt $ fromIntegral $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int)

data Diagonal = UL | UR | DL | DR deriving (Show)

diagonalDirection :: (Tail, Head) -> Diagonal
diagonalDirection ((xt, yt), (xh, yh))
    | xt < xh && yt < yh = UR
    | xt < xh && yt > yh = DR
    | xt > xh && yt < yh = UL
    | xt > xh && yt > yh = DL
    | otherwise = error "diagonalDirection: impossible"

move :: (Direction, Int) -> (Tail, Head) -> ([Tail], (Tail, Head))
move (d, n) (t, h)
    | n == 0 = (,) [] (t, h)
    | otherwise = first (t' :) $ move (d, pred n) (t', h')
  where
    h' = case d of
        U -> second succ h
        D -> second pred h
        L -> first pred h
        R -> first succ h
    t'
        | distance (t, h) == sqrt 2 && distance (t, h') > sqrt 2 = case diagonalDirection (t, h) of
            UL -> bimap pred succ t
            UR -> bimap succ succ t
            DL -> bimap pred pred t
            DR -> bimap succ pred t
        | distance (t, h') > 1 && distance (t, h') /= sqrt 2 = case d of
            U -> second succ t
            D -> second pred t
            L -> first pred t
            R -> first succ t
        | otherwise = t

readInput :: String -> (Direction, Int)
readInput = (read *** read) . splitAt 1

showGrid :: (Tail, Head) -> String
showGrid ((x1, y1), (x2, y2)) =
    let minx = 0
        maxx = 6
        miny = 0
        maxy = 5
     in unlines
            [ [ if x == x1 && y == y1
                then 'T'
                else
                    if x == x2 && y == y2
                        then 'H'
                        else
                            if x == 0 && y == 0
                                then 'O'
                                else '.'
              | x <- [minx .. maxx]
              ]
            | y <- [maxy, pred maxy .. miny]
            ]

test :: String
test = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

p1, p2 :: String -> String
p1 s = unsafePerformIO $ do
    let s' = map readInput $ lines s
    let (t, h) = ((0, 0), (0, 0)) :: (Tail, Head)
    putStrLn (showGrid (t, h))
    let k = foldl (\(ts, acc) m -> let (ts', n) = move m acc in (ts ++ ts', n)) ([], (t, h)) s'
    pure $ show $ length $ nub $ fst k
p2 = error
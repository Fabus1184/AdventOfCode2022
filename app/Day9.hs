{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Day9 (p1, p2) where

import Control.Arrow ((***))
import Data.Bifunctor (first, second)
import Data.List (nub)
import GHC.IO (unsafePerformIO)

data Direction = U | D | L | R deriving (Show)

type Position = (Int, Int)

instance Read Direction where
    readsPrec :: Int -> ReadS Direction
    readsPrec _ = \case
        'U' : s -> [(U, s)]
        'D' : s -> [(D, s)]
        'L' : s -> [(L, s)]
        'R' : s -> [(R, s)]
        _ -> []

move :: (Direction, Int) -> Position -> (Position, [Position])
move (d, n) h@(x, y)
    | n == 0 = (h, [])
    | otherwise = second (h' :) $ move (d, pred n) h'
  where
    h' = case d of
        U -> (x, succ y)
        D -> (x, pred y)
        L -> (pred x, y)
        R -> (succ x, y)

readInput :: String -> (Direction, Int)
readInput = (read *** read) . splitAt 1

showTrail :: [Position] -> String
showTrail trail =
    let minx = -20
        maxx = 20
        miny = -20
        maxy = 20
     in unlines
            . map
                ( \y ->
                    map
                        ( \x ->
                            if (x, y) == (0, 0)
                                then 'O'
                                else
                                    if (x, y) `elem` trail
                                        then 'X'
                                        else '.'
                        )
                        [minx .. maxx]
                )
            $ [maxy, pred maxy .. miny]

test :: String
test = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"

test2 :: String
test2 = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"

p1, p2 :: String -> String
p1 =
    show
        . pred
        . length
        . nub
        . fst
        . first ((0, 0) :)
        . foldl (\(hs, h) m -> let (h', hs') = move m h in (hs ++ init hs', h')) ([], (0, 0))
        . map readInput
        . lines
p2 s = unsafePerformIO $ do
    let s' = map readInput $ lines test2
    let (trail, _) = first (((0, 0) :) . drop 10) (foldl (\(hs, h) m -> let (h', hs') = move m h in (init hs' ++ hs, h')) ([], (0, 0)) s')
    putStrLn $ showTrail trail
    error ""
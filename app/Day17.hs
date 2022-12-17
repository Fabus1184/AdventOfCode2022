{-# LANGUAGE LambdaCase #-}

module Day17 (p1, p2) where

import Control.Lens (folded, maximumOf, none, _2)
import Data.Bifunctor (bimap, first, second)
import Data.Ix (inRange)
import Data.List.Extra (allSame, chunksOf)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, member, union)
import qualified Data.Set
import Data.Tuple.Extra (fst3)
import Lib ()

type Position = (Int, Int)
type Rock = Set Position
data Jet = L | R deriving (Show, Eq)

rocks :: [Rock]
rocks =
    map
        fromList
        [ [(0, 1), (1, 1), (2, 1), (3, 1)]
        , [(1, 1), (0, 2), (1, 2), (2, 2), (1, 3)]
        , [(0, 1), (1, 1), (2, 1), (2, 2), (2, 3)]
        , [(0, 1), (0, 2), (0, 3), (0, 4)]
        , [(0, 1), (1, 1), (0, 2), (1, 2)]
        ]

readJets :: String -> [Jet]
readJets =
    map
        ( \case
            '>' -> R
            '<' -> L
            _ -> error "readJets"
        )
        . concat
        . lines

placeRock :: Rock -> Set Position -> Rock
placeRock r rs =
    let topRock = fromMaybe 0 $ maximumOf (folded . _2) rs
     in Data.Set.map (bimap (+ 2) (+ (topRock + 3))) r

dropRock :: (Set Position, [Jet], [Rock]) -> [Set Position]
dropRock =
    map fst3
        . iterate
            ( \(rs, jets, r : rocks) ->
                let r' = placeRock r rs
                    (r'', jets') = go jets rs r'
                 in (r'' `union` rs, jets', rocks ++ [r])
            )
  where
    go :: [Jet] -> Set Position -> Rock -> (Rock, [Jet])
    go (j : jets) rs r =
        let r' =
                if (\p -> all (inRange (0, 6) . fst) p && none (`member` rs) p) (push j r)
                    then push j r
                    else r
         in if none (`member` rs) (fall r') && none ((< 1) . snd) (fall r')
                then go (jets ++ [j]) rs (fall r')
                else (r', jets ++ [j])
    go _ _ _ = error "go"

fall :: Rock -> Rock
fall = Data.Set.map (second pred)

push :: Jet -> Rock -> Rock
push L = Data.Set.map (first pred)
push R = Data.Set.map (first succ)

findPeriod :: Eq a => [a] -> Int
findPeriod xs = head $ filter (\i -> allSame $ take 3 $ chunksOf i $ drop i xs) [1 ..]

p1, p2 :: String -> Int
p1 s =
    let jets = readJets s
        ps = dropRock (mempty, jets, rocks) !! 2022
     in maximum (Data.Set.map snd ps)
p2 s =
    let jets = readJets s
        xs = map (maximum . Data.Set.map snd) $ tail $ dropRock (mempty, jets, rocks)
        ds = zipWith subtract xs (tail xs)
        p = findPeriod ds
        n = 1000000000000
     in (sum (take p ds) * (n `div` p)) + sum (take (n `mod` p) ds)

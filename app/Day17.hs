{-# LANGUAGE LambdaCase #-}

module Day17 (p1, p2) where

import Control.Lens (folded, maximum1Of, maximumOf, none, _2)
import Control.Monad (replicateM_)
import Control.Monad.Trans.State.Strict (State, execState, get, put)
import Data.Bifunctor (Bifunctor (second), bimap, first)
import Data.Ix (inRange)
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, member, union)
import qualified Data.Set
import Lib ()
import System.IO.Unsafe (unsafePerformIO)

type Position = (Int, Integer)
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

dropRock :: State (Set Position, [Jet], [Rock]) ()
dropRock =
    do
        (rs, jets, r : rocks) <- get
        let r' = placeRock r rs
        let (r'', jets') = go jets rs r'
         in put (r'' `union` rs, jets', rocks ++ [r])
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

test :: String
test = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

p1, p2 :: String -> Integer
p1 s = do
    let jets = readJets s
    let (ps, _, _) = execState (replicateM_ 2022 dropRock) (mempty, jets, rocks)
    maximum (Data.Set.map snd ps)
p2 s = unsafePerformIO $ do
    let jets = readJets test
    let (ps, _, _) = execState (replicateM_ 1000000000000 dropRock) (mempty, jets, rocks)
    print $ maximum (Data.Set.map snd ps)
    error ""

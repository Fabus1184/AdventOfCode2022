{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day16 (p1, p2) where

import Control.Lens (folded, ix, makeLenses, sumOf, (&), (+~), (-~), (.~), (^.))
import Control.Monad (unless, when)
import Control.Monad.Trans.State.Strict (State, execState, get, modify)
import Data.Char (isDigit)
import Data.List.Extra (splitOn)
import Data.Map.Strict (Map, fromList, insert, (!))
import Prelude hiding (lookup)

type Position = String

data Valve = Valve
    { _name :: !Position
    , _flowrate :: !Int
    , _nexts :: ![Position]
    }
    deriving (Show, Eq, Ord)
$(makeLenses ''Valve)

data Player = Player
    { _position :: !Position
    , _locked :: !Bool
    }
    deriving (Show, Eq, Ord)
$(makeLenses ''Player)

data Cave = Cave
    { _players :: [Player]
    , _time :: !Int
    , _released :: !Int
    , _valves :: !(Map Position Valve)
    }
    deriving (Show, Eq, Ord)
$(makeLenses ''Cave)

readValves :: String -> Map Position Valve
readValves =
    fromList
        . map
            ( (\x -> (,x) $ x ^. name)
                . ( \s' ->
                        let ws = words s'
                            nexts' = map (take 2) (words $ drop 1 (splitOn "valve" s' !! 1))
                         in Valve (ws !! 1) (read $ filter isDigit s') nexts'
                  )
            )
        . lines

{-# INLINE possibleMoves #-}
possibleMoves :: Int -> Int -> Cave -> [Cave]
possibleMoves t p !c =
    concat $
        concatMap
            ( \n ->
                [ [ c
                    & time +~ 1
                    & player . locked .~ False
                  | (c ^. players) !! p ^. locked
                  ]
                , [ c
                    & time +~ 1
                    & player . position .~ n
                  | not ((c ^. players) !! p ^. locked)
                  ]
                , [ c
                    & time +~ 1
                    & player . position .~ n
                    & player . locked .~ True
                    & valves . ix n . flowrate .~ 0
                    & released +~ valve' n ^. flowrate * (t - time' - 2)
                  | not ((c ^. players) !! p ^. locked)
                  , valve' n ^. flowrate /= 0
                  ]
                ]
            )
            pnexts'
  where
    player = players . ix p
    valve' _p = (c ^. valves) ! _p
    time' = c ^. time
    pnexts' = valve' ((c ^. players) !! p ^. position) ^. nexts

findAllPaths1 :: Cave -> State (Map Int Int) ()
findAllPaths1 !c
    | c ^. time > 30 = pure ()
    | otherwise = do
        m' <- get
        unless (c ^. released < m' ! (time' - 4)) $! do
            when (c ^. released > m' ! time') $! modify $! insert time' (c ^. released)
            unless (sumOf (folded . flowrate) (c ^. valves) == 0) $! mapM_ findAllPaths1 (possibleMoves 30 0 c)
  where
    time' = c ^. time

findAllPaths2 :: Cave -> State (Map Int Int) ()
findAllPaths2 !c
    | c ^. time > 25 = pure ()
    | otherwise = do
        m' <- get
        unless (c ^. released < m' ! (time' - 4)) $! do
            when (c ^. released > m' ! time') $! modify $! insert time' (c ^. released)
            unless (sumOf (folded . flowrate) (c ^. valves) == 0) $! do
                mapM_ findAllPaths2 $ [e | p <- possibleMoves 26 0 c, e <- possibleMoves 26 1 (p & time -~ 1)]
  where
    time' = c ^. time

p1, p2 :: String -> Int
p1 =
    maximum
        . (`execState` fromList (zip [-4 .. 30] $! repeat 0))
        . findAllPaths1
        . Cave [Player "AA" False] 0 0
        . readValves
p2 =
    maximum
        . (`execState` fromList (zip [-4 .. 26] $! repeat 0))
        . findAllPaths2
        . Cave [Player "AA" False, Player "AA" False] 0 0
        . readValves

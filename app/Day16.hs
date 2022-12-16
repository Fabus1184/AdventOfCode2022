{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day16 (p1, p2) where

import Control.Lens (at, folded, makeLenses, view, (%~), (&), (+~), (.~), (<&>), (?~), (^.), _Just)
import Control.Lens.Fold (sumOf)
import Control.Monad (unless, when)
import Control.Monad.Trans.State.Lazy (State, execState, get, modify)
import Data.Char (isDigit)
import Data.List.Extra (splitOn)
import Data.Map.Strict (Map, fromList, insert, (!))
import Lib (if')
import Prelude hiding (lookup)

data Valve = Valve
    { _name :: String
    , _flowrate :: Int
    , _next :: [Valve]
    }
$(makeLenses ''Valve)

data Player = Player
    { _position :: Valve
    , _locked :: Bool
    }
$(makeLenses ''Player)

data Cave = CS
    { _players :: [Player]
    , _time :: Int
    , _released :: Int
    }
$(makeLenses ''Cave)

readValves :: String -> [Valve]
readValves =
    map
        ( \s ->
            let ws = words s
                nexts = map (take 2) (words $ drop 1 (splitOn "valve" s !! 1))
             in Valve
                    (ws !! 1)
                    (read $ filter isDigit s)
                    (filter ((`elem` nexts) . view name) $ readValves s)
        )
        . lines

findAllPaths :: Cave -> State (Map Int Int) ()
findAllPaths cs
    | cs ^. time > 30 = pure ()
    | all ((== 0) . view flowrate) (cs ^. valves) = pure ()
    | otherwise = do
        m' <- get
        unless (cs ^. released < m' ! (cs ^. time - 3)) $ do
            when (cs ^. released > m' ! (cs ^. time)) $ modify (insert (cs ^. time) (cs ^. released))
            let curr = last (cs ^. path)
                nexts = (cs ^. valves) ! curr ^. next
            mapM_ findAllPaths $
                concatMap
                    ( \nm ->
                        (cs & path %~ (++ [nm]) & time %~ succ)
                            : [ cs
                                & path %~ (++ [nm])
                                & time +~ 2
                                & released +~ (cs ^. valves) ! nm ^. flowrate * (30 - cs ^. time - 2)
                                & valves . at nm ?~ ((cs ^. valves) ! nm & flowrate .~ 0)
                              | (cs ^. valves) ! nm ^. flowrate /= 0
                              ]
                    )
                    nexts

findAllPaths' :: Int -> Cave -> State (Map Int Int) ()
findAllPaths' n cs2
    | cs2 ^. time' > n = pure ()
    | otherwise = do
        m' <- get
        unless (cs2 ^. released' < m' ! (cs2 ^. time' - 3)) $ do
            when (cs2 ^. released' > m' ! (cs2 ^. time')) $ modify (insert (cs2 ^. time') (cs2 ^. released'))
            let currMe = last (cs2 ^. pathMe)
                currElephant = last (cs2 ^. pathElephant)
                nextsMe = if' (cs2 ^. lockMe) (const [currMe]) $ (cs2 ^. valves') ! currMe ^. next
                nextsElephant = if' (cs2 ^. lockElephant) (const [currElephant]) $ (cs2 ^. valves') ! currElephant ^. next
            if sumOf (folded . flowrate) (cs2 ^. valves') == 0
                then pure ()
                else
                    mapM_ (findAllPaths n) $
                        concatMap
                            ( \(nextNameMe, nextNameElephant) ->
                                let li = cs2 ^. lockMe
                                    le = cs2 ^. lockElephant
                                 in map
                                        ( $
                                            cs2
                                                & pathMe %~ (++ [nextNameMe])
                                                & pathElephant %~ (++ [nextNameElephant])
                                                & time' +~ 1
                                        )
                                        . concat
                                        $ [ [lockMe .~ False <&> lockElephant .~ False]
                                          , concat
                                                [ [ released'
                                                        +~ (cs2 ^. valves')
                                                        ! name
                                                        ^. flowrate
                                                        * (26 - cs2 ^. time' - 2)
                                                        <&> valves' . at name ?~ ((cs2 ^. valves') ! name & flowrate .~ 0)
                                                        <&> lockMe .~ True
                                                        <&> lockElephant .~ False
                                                  ]
                                                | name <- [nextNameMe, nextNameElephant]
                                                , (cs2 ^. valves') ! name ^. flowrate /= 0
                                                ]
                                          , [ released' +~ (cs2 ^. valves') ! nextNameElephant ^. flowrate * (26 - cs2 ^. time' - 2)
                                                <&> valves' . at nextNameElephant ?~ ((cs2 ^. valves') ! nextNameElephant & flowrate .~ 0)
                                                <&> lockElephant .~ True
                                                <&> lockMe .~ False
                                            | (cs2 ^. valves') ! nextNameElephant ^. flowrate /= 0
                                            , not le
                                            ]
                                          , [ released' +~ (cs2 ^. valves') ! nextNameMe ^. flowrate * (26 - cs2 ^. time' - 2)
                                                <&> released' +~ (cs2 ^. valves') ! nextNameElephant ^. flowrate * (26 - cs2 ^. time' - 2)
                                                <&> valves' . at nextNameElephant . _Just . flowrate .~ 0
                                                <&> valves' . at nextNameMe . _Just . flowrate .~ 0
                                                <&> lockMe .~ True
                                                <&> lockElephant .~ True
                                            | (cs2 ^. valves') ! nextNameMe ^. flowrate /= 0
                                            , (cs2 ^. valves') ! nextNameElephant ^. flowrate /= 0
                                            , not (cs2 ^. lockMe)
                                            , not (cs2 ^. lockElephant)
                                            , nextNameMe /= nextNameElephant
                                            ]
                                          ]
                            )
                            [(a, b) | a <- nextsMe, b <- nextsElephant]

f :: (Map String Valve -> State (Map Int Int) ()) -> String -> Int
f s =
    maximum
        . (`execState` fromList [(n, 0) | n <- [-10 .. 30]])
        . s
        . fromList
        . map ((\x -> (x ^. name, x)) . read)
        . lines

p1, p2 :: String -> Int
p1 = f $ \x -> findAllPaths $ CS x ["AA"] 0 0
p2 = f $ \x -> findAllPaths' $ CS2 x ["AA"] ["AA"] 0 False False 0

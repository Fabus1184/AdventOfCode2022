{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Day7 (p1, p2) where

import Control.Lens (ix, makeLenses, (%=), (%~), (&), (.=), (^.), _1, _2, _3)
import Control.Monad (unless)
import Control.Monad.Trans.State (State, execState, get)
import Data.List (findIndex)
import Data.Tuple.Extra (thd3)
import Lib ()

data Dir a = Dir
    { _dirName :: String
    , _dirFiles :: [a]
    , _dirDirs :: [Dir a]
    }
    deriving (Foldable)
$(makeLenses ''Dir)

type FileSystem = Dir Int
type Path = [String]

insertFile :: Path -> Int -> FileSystem -> FileSystem
insertFile [] f fs = fs & dirFiles %~ (f :)
insertFile (x : xs) f fs =
    fs & case findIndex ((== x) . (^. dirName)) (fs ^. dirDirs) of
        Nothing -> dirDirs %~ (insertFile xs f (Dir x [] []) :)
        Just i' -> (dirDirs . ix i') %~ insertFile xs f

parse :: State (Path, [String], FileSystem) ()
parse = do
    (path, ('$' : ' ' : cmd) : xs, _) <- get
    _2 .= xs
    case words cmd of
        ["cd", ".."] -> _1 %= init
        ["cd", dir'] -> _1 %= (++ [dir'])
        ["ls"] -> do
            let entries = takeWhile ((/= '$') . head) xs
            mapM_ ((_3 %=) . insertFile path . (read . head . words)) . filter ((/= "dir") . head . words) $ entries
            _2 .= drop (length entries) xs
        _ -> error $ "Unknown command: " <> cmd
    k' <- (^. _2) <$> get
    unless (null k') parse

dirs :: Dir a -> [Dir a]
dirs fs = fs : concatMap dirs (fs ^. dirDirs)

readInput :: String -> FileSystem
readInput = thd3 . execState parse . ([],,Dir "/" [] []) . tail . lines

p1, p2 :: String -> String
p1 = show . sum . filter (<= 100000) . map sum . dirs . readInput
p2 s = show . minimum . filter (>= sum (readInput s) - 40000000) . map sum . dirs . readInput $ s
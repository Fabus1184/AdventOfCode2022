{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day14 (p1, p2) where

import Control.Lens (makeLenses, view, views, (%~), (&), (^.))
import Control.Monad (liftM2, liftM4)
import Data.Char (isDigit)
import Data.List.Extra (find, nubOrd)
import Data.Set (Set, fromList, insert, notMember, size)
import Text.ParserCombinators.ReadP (char, munch1, readP_to_S, sepBy, string)

type Position = (Int, Int)
newtype Path = Path [Position]
data Cave = Cave
    { _stuff :: Set Position
    , _nRocks :: Int
    , _abyss :: Int
    , _nabyss :: Int
    }
$(makeLenses ''Cave)

instance Read Path where
    readsPrec :: Int -> ReadS Path
    readsPrec _ = readP_to_S $ Path <$> sepBy (liftM2 (,) (read <$> munch1 isDigit <* char ',') (read <$> munch1 isDigit)) (string " -> ")

readInput :: String -> Cave
readInput = liftM4 Cave fromList length (succ . maximum . map snd) (const 0) . nubOrd . concatMap (tracePath . read) . lines
  where
    tracePath :: Path -> [Position]
    tracePath (Path ((x1, y1) : (x2, y2) : ps))
        | x1 == x2 = map (x1,) [min y1 y2 .. max y1 y2] ++ tracePath (Path ((x2, y2) : ps))
        | y1 == y2 = map (,y1) [min x1 x2 .. max x1 x2] ++ tracePath (Path ((x2, y2) : ps))
    tracePath (Path ps) = ps

simulateWhile :: (Cave -> Bool) -> Cave -> Cave
simulateWhile p = head . dropWhile p . iterate (fallSand (500, 0))
  where
    fallSand :: Position -> Cave -> Cave
    fallSand (x, y) c'
        | y == c' ^. abyss = c' & stuff %~ insert (x, y) & nabyss %~ succ
        | otherwise = maybe (c' & stuff %~ insert (x, y)) (`fallSand` c') (find (`notMember` view stuff c') [(x, succ y), (pred x, succ y), (succ x, succ y)])

p1, p2 :: String -> Int
p1 = liftM2 (\n -> views stuff $ pred . subtract n . size) (view nRocks) (simulateWhile $ views nabyss (== 0)) . readInput
p2 = liftM2 (\n -> views stuff $ subtract n . size) (view nRocks) (simulateWhile $ views stuff $ notMember (500, 0)) . readInput
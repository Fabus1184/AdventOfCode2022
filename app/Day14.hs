{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day14 (p1, p2) where

import Control.Applicative (liftA2)
import Control.Lens (makeLenses, (%~), (&), (^.))
import Data.Char (isDigit)
import Data.List.Extra (maximumOn, nubOrd)
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
    readsPrec _ = readP_to_S $ Path <$> sepBy (liftA2 (,) (read <$> munch1 isDigit <* char ',') (read <$> munch1 isDigit)) (string " -> ")

readInput :: String -> Cave
readInput s =
    let paths = nubOrd $ concatMap (tracePath . read) $ lines s
     in Cave (fromList paths) (length paths) (succ $ snd $ maximumOn snd paths) 0
  where
    tracePath :: Path -> [Position]
    tracePath (Path ((x1, y1) : (x2, y2) : ps))
        | x1 == x2 = map (x1,) [min y1 y2 .. max y1 y2] ++ tracePath (Path ((x2, y2) : ps))
        | y1 == y2 = map (,y1) [min x1 x2 .. max x1 x2] ++ tracePath (Path ((x2, y2) : ps))
        | otherwise = error "invalid path"
    tracePath (Path ps) = ps

simulateWhile :: (Cave -> Bool) -> Cave -> Cave
simulateWhile p c = head $ dropWhile p $ iterate (fallSand (500, 0)) c

fallSand :: Position -> Cave -> Cave
fallSand (x, y) c
    | y == c ^. abyss = c & stuff %~ insert (x, y) & nabyss %~ succ
    | isNotStuff (x, y + 1) = fallSand (x, y + 1) c
    | isNotStuff (x - 1, y + 1) = fallSand (x - 1, y + 1) c
    | isNotStuff (x + 1, y + 1) = fallSand (x + 1, y + 1) c
    | otherwise = c & stuff %~ insert (x, y)
  where
    isNotStuff :: Position -> Bool
    isNotStuff p = p `notMember` (c ^. stuff)

p1, p2 :: String -> Int
p1 = liftA2 (\n -> pred . subtract n . size . (^. stuff)) (^. nRocks) (simulateWhile ((== 0) . (^. nabyss))) . readInput
p2 = liftA2 (\n -> subtract n . size . (^. stuff)) (^. nRocks) (simulateWhile (((500, 0) `notMember`) . (^. stuff))) . readInput
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Day15 (p1, p2) where

import Control.Lens (makeLenses, view)
import Data.Char (isDigit)
import Data.List.Extra (groupBy)
import Data.Maybe (fromJust, mapMaybe)
import Data.Range (Bound (..), Range (..), difference, mergeRanges, (+=+))
import Lib ((<$$>))
import Linear (V2 (V2), det22, (*^), (^+^), _y)

data Circle a = C
    { _pos :: V2 a
    , _rad :: a
    }
    deriving (Show, Eq, Functor)
$(makeLenses ''Circle)

readInput :: String -> [Circle Int]
readInput =
    map
        ( (\[_, x, _, y, _, x', _, y'] -> let (p, c) = (,) (V2 x y) (V2 x' y') in C p $ sum $ abs <$> p - c)
            . map read
            . groupBy (\a b -> isDigit a == isDigit b)
        )
        . lines

readBeacons :: String -> [V2 Int]
readBeacons = map ((\[_, _, _, _, _, x, _, y] -> V2 x y) . map read . groupBy (\a b -> isDigit a == isDigit b)) . lines

circleDistance :: Num a => Circle a -> Circle a -> a
circleDistance (C m r) (C m' r') = sum (abs <$> m - m') - r - r'

lineIntersection :: (Eq a, Fractional a) => (V2 a, V2 a) -> (V2 a, V2 a) -> Maybe (V2 a)
lineIntersection (p, v) (p', v') =
    if det22 (V2 v v') == 0
        then Nothing
        else Just (p ^+^ ((det22 (V2 (p' - p) v') / det22 (V2 v v')) *^ v))

unit :: Floating a => V2 a -> V2 a
unit (V2 x y) = let d = sqrt (x * x + y * y) in V2 (x / d) (y / d)

circleAtY :: Int -> Circle Int -> Maybe (Range Int)
circleAtY y' (C (V2 x y) r) =
    if r < abs (y' - y)
        then Nothing
        else Just $ (x - (r - abs (y' - y))) +=+ (x + (r - abs (y' - y)))

p1, p2 :: String -> Int
p1 s =
    let y = 2000000
        beacons = filter ((== y) . view _y) $ readBeacons s
     in foldr (\(SpanRange (Bound a _) (Bound b _)) -> (+) $ abs (a - b)) 0
            . (\k -> foldl (\acc (V2 x _) -> acc `difference` [x +=+ x]) k beacons)
            . foldl (\acc -> mergeRanges . (: acc)) mempty
            . mapMaybe (circleAtY y)
            . readInput
            $ s
p2 s =
    let (s1, s2) = (`div` 2) . length >>= splitAt $ readInput s
        [(C a r, C b _), (C c r', C d _)] =
            [ (fromIntegral <$> v, fromIntegral <$> v') :: (Circle Double, Circle Double)
            | v <- s1
            , v' <- s2
            , circleDistance v v' == 2
            ]
        f = fromIntegral @Int . round
        ar = (f <$> unit (b - a))
        m1 = f <$> a + succ r / 2 *^ ar
        cr = (f <$> unit (d - c))
        m2 = f <$> c + succ r' / 2 *^ cr
     in (\(V2 x y) -> x * 4000000 + y) $ fromJust (round <$$> lineIntersection (m1, cr) (m2, ar))

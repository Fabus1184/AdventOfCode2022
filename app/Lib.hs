{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Lib where

import Control.Lens (
    Field1 (..),
    Field10 (..),
    Field2 (..),
    Field3 (..),
    Field4 (..),
    Field5 (..),
    Field6 (..),
    Field7 (..),
    Field8 (..),
    Field9 (..),
    Identity,
 )
import Control.Monad.Extra (concatMapM)
import Data.Tuple.Extra (both)
import LibTH (mktmap, mkttake, mktup, mkuntup)

data Showable = forall a. MyShow a => MkShowable a

instance MyShow Showable where
    myShow :: Showable -> String
    myShow (MkShowable a) = myShow a

class MyShow a where
    myShow :: a -> String

instance MyShow Int where
    myShow :: Int -> String
    myShow = show

instance MyShow String where
    myShow :: String -> String
    myShow = id

instance MonadFail Identity where
    fail :: String -> Identity a
    fail = error

instance Semigroup Int where
    (<>) :: Int -> Int -> Int
    (<>) = (+)

instance Monoid Int where
    mempty :: Int
    mempty = 0

instance Num a => Num ((,) a a) where
    (+) :: (a, a) -> (a, a) -> (a, a)
    (+) (x, y) (x', y') = (x + x', y + y')

    (*) :: (a, a) -> (a, a) -> (a, a)
    (*) = error "(*), (a, a)"

    (-) :: (a, a) -> (a, a) -> (a, a)
    (-) (x, y) (x', y') = (x - x', y - y')

    negate :: (a, a) -> (a, a)
    negate = both negate

    abs :: (a, a) -> (a, a)
    abs = both abs

    signum :: (a, a) -> (a, a)
    signum = error "signum (a, a)"

    fromInteger :: Integer -> (a, a)
    fromInteger x = (fromInteger x, fromInteger x)

$(concatMapM mktup [1 .. 10])
$(concatMapM mkuntup [1 .. 10])
$(concatMapM mktmap [1 .. 10])
$(concatMapM mkttake [1 .. 10])

infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

if' :: Bool -> (a -> a) -> a -> a
if' True f = f
if' False _ = id
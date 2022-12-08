{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
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
import LibTH (mktmap, mkttake, mktup, mkuntup)

instance MonadFail Identity where
    fail :: String -> Identity a
    fail = error

instance Semigroup Int where
    (<>) :: Int -> Int -> Int
    (<>) = (+)

instance Monoid Int where
    mempty :: Int
    mempty = error "mempty Int"

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
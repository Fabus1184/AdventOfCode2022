{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

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
 )
import Control.Monad.Extra (concatMapM)
import LibTH (mktmap, mkttake, mktup, mkuntup)

$(concatMapM mktup [1 .. 10])
$(concatMapM mkuntup [1 .. 10])
$(concatMapM mktmap [1 .. 10])
$(concatMapM mkttake [1 .. 10])

if' :: Bool -> (a -> a) -> a -> a
if' True f = f
if' False _ = id
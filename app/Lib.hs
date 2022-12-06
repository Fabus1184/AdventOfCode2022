{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Monad.Extra (concatMapM)
import LibTH (mktmap, mktup, mkuntup)

$(concatMapM mktup [1 .. 10])
$(concatMapM mkuntup [1 .. 10])
$(concatMapM mktmap [1 .. 10])

if' :: Bool -> (a -> a) -> a -> a
if' True f = f
if' False _ = id
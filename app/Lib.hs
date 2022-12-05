{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Monad.Extra (concatMapM)
import LibTH (mktmap, mktup)

$(concatMapM mktup [2 .. 10])
$(concatMapM mktmap [2 .. 10])

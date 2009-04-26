{-# OPTIONS -O2 -Wall #-}

module Data.MList.IO where

import Data.MList(MList(..))
import Data.MList.Control(while)
import System.IO(Handle, hGetLine, hIsEOF)

hGetLines :: Handle -> MList IO String
hGetLines handle = linesMList
    where
      linesMList = while (fmap not $ hIsEOF handle) (hGetLine handle)

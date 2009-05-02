{-# OPTIONS -O2 -Wall #-}

module Data.MList.IO where

import Data.MList(MList(..))
import Data.MList.Control(while)
import System.IO(Handle, hGetLine, hIsEOF)

-- System.IO.hGetLine is strict I/O, unlike System.IO.hGetContents
hGetLines :: Handle -> MList IO String
hGetLines handle = while (fmap not $ hIsEOF handle) (hGetLine handle)

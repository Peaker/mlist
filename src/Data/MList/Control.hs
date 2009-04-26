{-# OPTIONS -O2 -Wall #-}

module Data.MList.Control(while) where

import Data.MList(MList(..),MListItem(MCons,MNil))
import Control.Monad(liftM)

while :: Monad m => m Bool -> m a -> MList m a
while cond act = result
    where result = MList $ do
                     p <- cond
                     if p
                       then liftM (`MCons` result) act
                       else return MNil

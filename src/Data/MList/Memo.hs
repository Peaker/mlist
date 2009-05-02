{-# OPTIONS -O2 -Wall #-}

module Data.MList.Memo where

import Data.MList(MList(..), MListItem(..))
import Control.Concurrent.MVar(newMVar,modifyMVar)
import Control.Monad(liftM2)

-- TODO: Can this be expressed with mfoldr/mfoldr'?
memo :: MList IO a -> IO (MList IO a)
memo (MList acx) =
    do
      mv <- newMVar Nothing
      -- Return an mlist that calls fillMemo or getMemo on the mvar
      -- when the element is requested
      return . MList . modifyMVar mv $ maybe fillMemo getMemo
    where
      fillMemo = do
        cx <- acx
        res <- case cx of
                 MNil -> return MNil
                 MCons x xs -> (x `MCons`) `fmap` memo xs
        return (Just res, res)
      getMemo = return . liftM2 (,) Just id

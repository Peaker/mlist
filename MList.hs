{-# OPTIONS -O2 -Wall #-}

module Main where

import System.IO(FilePath, Handle, hGetLine, openFile, IOMode(ReadMode), hIsEOF)
import Control.Applicative(Applicative(..), liftA2)
import Control.Monad(liftM2)
import Data.Monoid(Monoid(..))

data Monad m => MListItem m a = MNil | MCons a (MList m a)
newtype Monad m => MList m a = MList { unMList :: m (MListItem m a) }

mlist :: Monad m => MListItem m a -> MList m a
mlist = MList . return

empty :: Monad m => MList m a
empty = mlist MNil

hGetLines :: Handle -> MList IO String
hGetLines handle =
    let linesMList = MList $ do
          eof <- hIsEOF handle
          if eof
            then return MNil
            else do
              line <- hGetLine handle
              return $ MCons line linesMList
    in linesMList

singleton :: Monad m => a -> MList m a
singleton x = mlist $ MCons x empty

fromList :: Monad m => [a] -> MList m a
fromList = foldr item empty
    where
      item x r = mlist $ MCons x r

zipMList :: Monad m => (a -> b -> c) -> MList m a -> MList m b -> MList m c
zipMList f (MList acx) (MList acy) =
    MList $ do
      cx <- acx
      case cx of
        MNil -> return MNil
        MCons x acxRest -> do
          cy <- acy
          return $
            case cy of
              MNil -> MNil
              MCons y acyRest ->
                  MCons (f x y) (zipMList f acxRest acyRest)

mapMList :: Monad m => (a -> m b) -> MList m a -> m [b]
mapMList f (MList acx) = do
  cx <- acx
  case cx of
    MNil -> return []
    MCons x acxRest -> liftM2 (:) (f x) (mapMList f acxRest)

forMList :: Monad m => MList m a -> (a -> m b) -> m [b]
forMList = flip mapMList

extract :: Monad m => MList m a -> m [a]
extract = mapMList return

mapMList_ :: Monad m => (a -> m ()) -> MList m a -> m ()
mapMList_ f (MList acx) = do
  cx <- acx
  case cx of
    MNil -> return ()
    MCons x acxRest -> f x >> mapMList_ f acxRest

forMList_ :: Monad m => MList m a -> (a -> m ()) -> m ()
forMList_ = flip mapMList_

instance Monad m => Functor (MList m) where
    fmap f (MList acx) =
        MList $ do
          cx <- acx
          return $
            case cx of
              MNil -> MNil
              MCons x acxRest -> MCons (f x) (fmap f acxRest)

append :: Monad m => MList m a -> MList m a -> MList m a
append (MList acx) mly@(MList acy) =
    MList $ do
      cx <- acx
      case cx of
        MNil -> acy
        MCons x acxRest ->
          return $ MCons x (append acxRest mly)

concatMList :: Monad m => MList m (MList m a) -> MList m a
concatMList (MList mcx) =
    MList $ do
      cx <- mcx
      case cx of
        MNil -> return MNil
        MCons x mcxRest ->
          unMList $ append x (concatMList mcxRest)

instance Monad m => Applicative (MList m) where
    pure = singleton
    mlf <*> mlx = concatMList $ fmap (`fmap` mlx) mlf

instance Monad m => Monad (MList m) where
    return = pure
    xs >>= f = concatMList $ fmap f xs

instance Monad m => Monoid (MList m a) where
    mempty = empty
    mappend = append

numbers :: Monad m => MList m Integer
numbers = fromList [0..]

enumerate :: Monad m => MList m a -> MList m (Integer, a)
enumerate = zipMList (,) numbers

mlistAction :: Monad m => m (MList m a) -> MList m a
mlistAction act =
    MList $ do
      ml <- act
      unMList ml

mlistForPath :: FilePath -> MList IO String
mlistForPath path =
    let linesListOfPath = fmap hGetLines $ openFile path ReadMode
    in mlistAction linesListOfPath
      

main :: IO ()
main = do
    let blah = mlistForPath "test/blah"
        bleh = mlistForPath "test/bleh"
    forMList_ (liftA2 (++) blah bleh) $ \line -> do
      print line
    return ()

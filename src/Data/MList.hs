{-# OPTIONS -O2 -Wall #-}

module Data.MList(MList(..)
                 ,MListItem(..)
                 ,empty
                 ,cons
                 ,singleton
                 ,fromList
                 ,zipWith
                 ,mlistFold
                 ,sfoldr
                 ,mnSequence
                 ,mnSequence_
                 ,mnmapM
                 ,mnmapM_
                 ,mnforM
                 ,mnforM_
                 ,mSequence
                 ,mSequence_
                 ,mmapM
                 ,mmapM_
                 ,mforM
                 ,mforM_
                 ,extract
                 ,concat
                 ,mmerge
                 ,numbers
                 ,enumerate) where

import Prelude hiding (zipWith, concat)

import Control.Applicative(Applicative(..))
import Control.Monad(liftM2,join)
import Data.Monoid(Monoid(..))

data Monad m => MListItem m a = MNil | MCons a (MList m a)
newtype Monad m => MList m a = MList { unMList :: m (MListItem m a) }

mlist :: Monad m => MListItem m a -> MList m a
mlist = MList . return

empty :: Monad m => MList m a
empty = mlist MNil

cons :: Monad m => a -> MList m a -> MList m a
cons x = mlist . MCons x

singleton :: Monad m => a -> MList m a
singleton x = cons x empty

fromList :: Monad m => [a] -> MList m a
fromList = foldr cons empty

zipWith :: Monad m => (a -> b -> c) -> MList m a -> MList m b -> MList m c
zipWith f (MList acx) (MList acy) =
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
                  MCons (f x y) (zipWith f acxRest acyRest)

mlistFold :: Monad m => (a -> b -> m b) -> m b -> MList m a -> m b
mlistFold consFunc nilFunc (MList acx) = do
  cx <- acx
  case cx of
    MNil -> nilFunc
    MCons x acxRest -> mlistFold consFunc nilFunc acxRest >>=
                       consFunc x

sfoldr :: Monad m => (a -> b -> b) -> b -> MList m a -> m b
sfoldr consFunc nilFunc = mlistFold ((return .) . consFunc) (return nilFunc)

mnSequence :: (Monad m, Monad n) => MList m (n a) -> m (n [a])
mnSequence = sfoldr (liftM2 (:)) (return [])

mnSequence_ :: (Monad m, Monad n) => MList m (n a) -> m (n ())
mnSequence_ = sfoldr (>>) (return ())

mnmapM :: (Monad m, Monad n) => (a -> n b) -> MList m a -> m (n [b])
mnmapM f = mnSequence . fmap f

mnforM :: (Monad m, Monad n) => MList m a -> (a -> n b) -> m (n [b])
mnforM = flip mnmapM

mnmapM_ :: (Monad m, Monad n) => (a -> n b) -> MList m a -> m (n ())
mnmapM_ f = mnSequence_ . fmap f

mnforM_ :: (Monad m, Monad n) => MList m a -> (a -> n b) -> m (n ())
mnforM_ = flip mnmapM_


mSequence :: Monad m => MList m (m a) -> m [a]
mSequence = join . mnSequence

mSequence_ :: Monad m => MList m (m a) -> m ()
mSequence_ = join . mnSequence_

mmapM :: Monad m => (a -> m b) -> MList m a -> m [b]
mmapM = (join.) . mnmapM

mforM :: Monad m => MList m a -> (a -> m b) -> m [b]
mforM = (join.) . mnforM

mmapM_ :: Monad m => (a -> m b) -> MList m a -> m ()
mmapM_ = (join.) . mnmapM_

mforM_ :: Monad m => MList m a -> (a -> m b) -> m ()
mforM_ = (join.) . mnforM_


extract :: Monad m => MList m a -> m [a]
extract = sfoldr (:) []

instance Monad m => Monoid (MList m a) where
    mempty = empty
    mappend xs ys = mmerge $ sfoldr cons ys xs

concat :: Monad m => MList m (MList m a) -> MList m a
concat = mmerge . sfoldr mappend mempty

mmerge :: Monad m => m (MList m a) -> MList m a
mmerge act = MList $ act >>= unMList

numbers :: Monad m => MList m Integer
numbers = fromList [0..]

enumerate :: Monad m => MList m a -> MList m (Integer, a)
enumerate = zipWith (,) numbers

instance Monad m => Functor (MList m) where
    fmap f = mmerge . sfoldr (cons . f) empty

instance Monad m => Applicative (MList m) where
    pure = singleton
    (<*>) = liftM2 id

instance Monad m => Monad (MList m) where
    return = pure
    xs >>= f = concat $ fmap f xs

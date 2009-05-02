{-# OPTIONS -O2 -Wall #-}

-- TODO:
-- 1. For MList is to be "lazy" two things must hold:
--
--    A. The actions of the MList have to be interlaced with the
--       actions of the sequence.  This means that the sequence must
--       be of a single monad.
--
--    B. Ability to "break" a loop:
--       mwhile :: MList m (m (Bool, a)) -> m ([a], MList m (m (Bool, a)))   OR:
--       mfoldr :: (a -> m b -> m b) -> m b -> MList m a -> m b
--
--       foldl is also interesting:
--       mfoldl :: (m b -> a -> m b) -> m b -> MList m a -> m b
--       mfoldl f z xs = do { f z
--       foldl ::  (b -> a -> b) -> b -> [a] -> b
--       foldl f z [] = z
--       foldl f z (x:xs) = foldl f (f z x) xs

module Data.MList(MList(..)
                 ,MListItem(..)
                 ,empty
                 ,cons
                 ,singleton
                 ,fromList
                 ,zipWith

                 ,mfoldr
                 ,condense
                 -- ,msequence
                 -- ,msequence_
                 -- ,mmapM
                 -- ,mmapM_
                 -- ,mforM
                 -- ,mforM_

                 ,toList
                 ,append
                 ,concat
                 ,mmerge
                 ,numbers
                 ,enumerate) where

import Prelude hiding (zipWith, concat)

import Control.Applicative(Applicative(..))
import Control.Monad(liftM,liftM2)
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
        MCons x xs -> do
          cy <- acy
          return $
            case cy of
              MNil -> MNil
              MCons y acyRest ->
                  MCons (f x y) (zipWith f xs acyRest)

-- bind the monadic effect before the first item in the list.
mmerge :: Monad m => m (MList m a) -> MList m a
mmerge act = MList $ act >>= unMList

mfoldr :: Monad m => (a -> m b -> m b) -> m b -> MList m a -> m b
mfoldr consFunc nilFunc (MList acx) = do
  cx <- acx
  case cx of
    MNil -> nilFunc
    MCons x xs -> consFunc x $ mfoldr consFunc nilFunc xs

append :: Monad m => MList m a -> MList m a -> MList m a
xs `append` ys = mmerge $ mfoldr consFunc nilFunc xs
    where
      nilFunc = return ys
      consFunc x rest = return . cons x . mmerge $ rest

condense :: Monad m => MList m (m a) -> MList m a
condense = mmerge . mfoldr (liftM2 cons) (return empty)

-- strict foldr -- executes the entire list in any case!
sfoldr :: Monad m => (a -> b -> b) -> b -> MList m a -> m b
sfoldr consFunc nilFunc = mfoldr liftConsFunc liftNilFunc
    where
      liftConsFunc x act = liftM (x `consFunc`) act
      liftNilFunc = return nilFunc

-- msequence :: Monad m => MList m (m a) -> m [a]
-- mSequence_ :: Monad m => MList m (m a) -> m ()
-- mmapM :: Monad m => (a -> m b) -> MList m a -> m [b]
-- mforM :: Monad m => MList m a -> (a -> m b) -> m [b]
-- mmapM_ :: Monad m => (a -> m b) -> MList m a -> m ()
-- mforM_ :: Monad m => MList m a -> (a -> m b) -> m ()


-- strict
toList :: Monad m => MList m a -> m [a]
toList = sfoldr (:) []
      
instance Monad m => Monoid (MList m a) where
    mempty = empty
    mappend = append

concat :: Monad m => MList m (MList m a) -> MList m a
-- TODO: sfoldr is strict! use a lazy implementation
concat = mmerge . sfoldr mappend mempty

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

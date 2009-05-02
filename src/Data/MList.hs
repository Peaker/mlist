{-# OPTIONS -O2 -Wall #-}

module Data.MList(MList(..)
                 ,MListItem(..)
                 ,empty
                 ,cons
                 ,singleton

                 ,fromList
                 ,toList
                 ,execute

                 ,zipWith
                 ,take
                 ,repeat
                 ,replicate
                 ,mrepeat
                 ,mreplicate
                 ,cycle

                 ,mfoldr
                 ,mfoldr'
                 ,map
                 ,condense
                 ,msequence
                 ,msequence_
                 ,mmap
                 ,mfor

                 ,append
                 ,concat
                 ,mmerge) where

import Prelude hiding (zipWith,concat,take,replicate,repeat,cycle,map)

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

-- strict foldr -- executes the entire list in any case!
sfoldr :: Monad m => (a -> b -> b) -> b -> MList m a -> m b
sfoldr consFunc nilFunc = mfoldr liftConsFunc liftNilFunc
    where
      liftConsFunc x act = liftM (x `consFunc`) act
      liftNilFunc = return nilFunc
-- strict
toList :: Monad m => MList m a -> m [a]
toList = sfoldr (:) []
execute :: Monad m => MList m a -> m ()
execute = sfoldr ((const . const) ()) ()

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

mfoldr' :: Monad m => (a -> MList m b -> MList m b) -> MList m b -> MList m a -> MList m b
mfoldr' consFunc nilFunc = mmerge . mfoldr liftConsFunc liftNilFunc
    where
      liftNilFunc = return nilFunc
      liftConsFunc x rest = return . consFunc x . mmerge $ rest

map :: Monad m => (a -> b) -> MList m a -> MList m b
map f = mfoldr' (cons . f) empty

append :: Monad m => MList m a -> MList m a -> MList m a
xs `append` ys = mfoldr' cons ys xs

concat :: Monad m => MList m (MList m a) -> MList m a
concat = mfoldr' append empty

condense :: Monad m => MList m (m a) -> MList m a
condense = mfoldr' consFunc empty
    where
      consFunc x xs = mmerge . liftM (`cons` xs) $ x

take :: (Integral i, Monad m) => i -> MList m a -> MList m a
take i (MList acx)
    | i <= 0    = empty
    | otherwise = MList $ do
                    cx <- acx
                    case cx of
                      MNil -> return MNil
                      MCons x rest -> return . MCons x . take (i-1) $ rest

repeat :: Monad m => a -> MList m a
repeat x = xs
    where xs = cons x xs

replicate :: (Monad m, Integral i) => i -> a -> MList m a
replicate n = take n . repeat

mrepeat :: Monad m => m a -> MList m a
mrepeat ax = xs
    where xs = MList $ liftM (`MCons` xs) ax

mreplicate :: (Monad m, Integral i) => i -> m a -> MList m a
mreplicate n = take n . mrepeat

cycle :: Monad m => MList m a -> MList m a
cycle xs = cxs
    where cxs = xs `append` cxs

msequence :: Monad m => MList m (m a) -> m [a]
msequence = toList . condense

msequence_ :: Monad m => MList m (m a) -> m ()
msequence_ = execute . condense

mmap :: Monad m => (a -> m b) -> MList m a -> MList m b
mmap f = condense . map f

mfor :: Monad m => MList m a -> (a -> m b) -> MList m b
mfor = flip mmap

instance Monad m => Monoid (MList m a) where
    mempty = empty
    mappend = append

instance Monad m => Functor (MList m) where
    fmap = map

instance Monad m => Applicative (MList m) where
    pure = return
    (<*>) = liftM2 id

instance Monad m => Monad (MList m) where
    return = singleton
    xs >>= f = concat . map f $ xs

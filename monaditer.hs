{-# OPTIONS -O2 -Wall #-}

module Main where

import System.IO(Handle, hGetLine, openFile, IOMode(ReadMode), hIsEOF)
import Control.Monad(liftM)

newtype Monad m => Iter m a = Iter (Maybe (a, m (Iter m a)))

empty :: Monad m => Iter m a
empty = Iter Nothing

full :: Monad m => (a, m (Iter m a)) -> Iter m a
full = Iter . Just

hNextLine :: Handle -> IO (Maybe String)
hNextLine handle = do
    eof <- hIsEOF handle
    if eof
      then return Nothing
      else return . Just =<< hGetLine handle

hLinesIter :: Handle -> IO (Iter IO String)
hLinesIter handle =
    let linesIter = do
          line <- hNextLine handle
          return . Iter $ flip (,) linesIter `fmap` line
    in linesIter

singletonIter :: Monad m => a -> Iter m a
singletonIter x = full $ (x, return empty)

pureIter :: Monad m => [a] -> Iter m a
pureIter = foldr item empty
    where
        item x r = full $ (x, return r)

zipWithIter :: Monad m => (a -> b -> c) -> Iter m a -> Iter m b -> Iter m c
zipWithIter f (Iter (Just (x, xRest))) (Iter (Just (y, yRest))) =
    full $ (f x y, combine)
        where
            combine = do
                xIter <- xRest
                yIter <- yRest
                return $ zipWithIter f xIter yIter
zipWithIter _ _ _ = empty

iterMapM :: Monad m => (a -> m b) -> Iter m a -> m [b]
iterMapM f (Iter m) = maybe (return []) handleJust m
    where handleJust (val, rest) = do
            x <- f val
            newIter <- rest
            xs <- iterMapM newIter f
            return (x:xs)

iterMapM_ :: Monad m => (a -> m ()) -> Iter m a -> m ()
iterMapM_ f (Iter m) = maybe (return ()) handleJust m
    where handleJust (val, rest) = do
            f val
            newIter <- rest
            iterMapM_ newIter f

iterMap :: (a -> b) -> Iter m a -> Iter m b
iterMap _ (Iter Nothing) = Iter Nothing
iterMap f (Iter (Just (x, rest))) = Iter (Just (f x, (liftM . iterMap) f rest))

instance Monad m => Functor (Iter m) where
    fmap = iterMap

iterAp :: Iter m (a -> b) -> Iter m a -> Iter m b
Iter (Just (f, fRest)) `iterAp` xs = do
    rxs <- iterMapM f xs
    fIterRest <- fRest
    iterAp fIterRest xs
_ `iterAp` _ = Iter Nothing

instance Monad m => Applicative (Iter m) where
    pure = singletonIter
    (<*>) = iterCartesian

-- instance Monad m => Monad (Iter m) where
--     return = singletonIter
--     Iter (Just (x, xrest)) >>= f  =
--         case f x of
--             Iter Nothing -> Iter Nothing
--             Iter (Just (y, yrest)) -> Iter (Just (y, rest))
--       where
--           rest = do
--               yiter <- yrest
--     _                      >>= _  = empty

main :: IO ()
main = do
    handle <- openFile "blah" ReadMode
    linesI <- hLinesIter handle
    let iLines = zipWithIter (,) (pureIter ([1..] :: [Integer])) linesI
    iterM iLines $ \line -> do
      print line
    return ()

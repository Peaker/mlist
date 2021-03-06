{-# OPTIONS -O2 -Wall #-}

import qualified Data.MList as MList
import qualified Data.MList.Control as MControl
import qualified Data.MList.Memo as MMemo
import Data.MList(MList)
import Data.MList.IO(hGetLines)
import Data.Monoid(Monoid(..))
import System.IO(FilePath,openFile,IOMode(ReadMode))

import Control.Applicative(liftA2)
import Control.Monad(liftM2)
import Control.Arrow(first)
import Control.Monad(forM_)

mlistForPath :: FilePath -> MList IO String
mlistForPath path = MList.mmerge linesListOfPath
    where handle = openFile path ReadMode
          linesListOfPath = fmap hGetLines handle

enumerate :: Monad m => MList m a -> MList m (Integer, a)
enumerate = MList.zipWith (,) (MList.fromList [0..])

main :: IO ()
main = do
  let xs = fmap fst . enumerate . MControl.forever $ print "Next Item"
      finite = MList.take (3::Int) xs

  putStrLn "Testing append..."
  let result1 = xs `MList.append` xs
  MList.MCons x1 _ <- MList.unMList result1
  print x1

  putStrLn "Testing memo..."
  memoresult1 <- MMemo.memo result1
  forM_ ['a'..'d'] . const $ do
      putStrLn "Getting first element of memoized list"
      MList.MCons mx1 _ <- MList.unMList memoresult1
      print mx1

  putStrLn "Testing condense ..."
  let result2 = MList.condense $ fmap print xs
  MList.MCons x2 _ <- MList.unMList result2
  print x2

  putStrLn "Testing cycle ..."
  let result3 = MList.take (5::Int) . MList.cycle $ finite
  MList.MCons x3 _ <- MList.unMList result3
  print x3
  print =<< MList.toList result3

  putStrLn "Testing files and instances ..."
  let enumStr = fmap (uncurry (++) . first ((" " ++) . (++ " ") . show)) . enumerate
      blah = enumStr . mlistForPath $ "test/blah"
      bleh = enumStr . fmap show $ finite
  putStrLn "Applicative ..."
  MList.execute . MList.mmap print $ liftA2 (++) blah bleh
  putStrLn "Monad and Monoid ..."
  MList.execute . MList.mmap print $ liftM2 mappend blah bleh
  return ()

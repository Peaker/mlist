{-# OPTIONS -O2 -Wall #-}

import qualified Data.MList as MList
import Data.MList(MList)
import Data.MList.IO(hGetLines)
import Data.Monoid(Monoid(..))
import System.IO(FilePath, openFile, IOMode(ReadMode))
import Control.Applicative(liftA2)
import Control.Arrow(first)
import Control.Monad(join)

mlistForPath :: FilePath -> MList IO String
mlistForPath path = MList.mmerge linesListOfPath
    where handle = openFile path ReadMode
          linesListOfPath = fmap hGetLines handle

main :: IO ()
main = do
  xs1 <- MList.mSequence . MList.fromList $ [return "a", return "b"]
  print xs1

  xs2 <- MList.extract . MList.enumerate . fmap (join (++)) . MList.fromList $ ["a","b","c"]
  print xs2

  let l1 = MList.fromList "abc"
      l2 = MList.fromList "def"
  xs3 <- MList.extract $ l1 `mappend` l2
  print xs3

  xs4 <- MList.extract . mlistForPath $ "test/blah"
  print xs4

  let enumStr = fmap (uncurry (++) . first ((" " ++) . (++ " ") . show)) . MList.enumerate
      blah = enumStr . mlistForPath $ "test/blah"
      bleh = enumStr . mlistForPath $ "test/bleh"
  MList.mforM_ (liftA2 (++) blah bleh) print
  return ()

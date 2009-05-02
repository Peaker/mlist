{-# OPTIONS -O2 -Wall #-}

import qualified Data.MList as MList
import qualified Data.MList.Control as MControl
import Data.MList(MList)
-- import Data.Monoid(Monoid(..))
-- import System.IO(FilePath,openFile,IOMode(ReadMode))

-- import Control.Applicative(liftA2)
-- import Control.Arrow(first)

-- mlistForPath :: FilePath -> MList IO String
-- mlistForPath path = MList.mmerge linesListOfPath
--     where handle = openFile path ReadMode
--           linesListOfPath = fmap hGetLines handle

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

  putStrLn "Testing condense ..."
  let result2 = MList.condense $ fmap print xs
  MList.MCons x2 _ <- MList.unMList result2
  print x2

  putStrLn "Testing cycle ..."
  let result3 = MList.take (5::Int) . MList.cycle $ finite
  MList.MCons x3 _ <- MList.unMList result3
  print x3
  print =<< MList.toList result3

  -- putStrLn "Testing concat..."
  -- let result2 = MList.concat $ fmap print ls
  -- MList.MCons line2 _ <- MList.unMList result2
  -- print line2

  return ()
  -- xs1 <- MList.mSequence . MList.fromList $ [return "a", return "b"]
  -- print xs1

  -- xs2 <- MList.extract . enumerate . fmap (join (++)) . MList.fromList $ ["a","b","c"]
  -- print xs2

  -- let l1 = MList.fromList "abc"
  --     l2 = MList.fromList "def"
  -- xs3 <- MList.extract $ l1 `mappend` l2
  -- print xs3

  -- xs4 <- MList.toList . mlistForPath $ "test/blah"
  -- print xs4

  -- let enumStr = fmap (uncurry (++) . first ((" " ++) . (++ " ") . show)) . enumerate
  --     blah = enumStr . mlistForPath $ "test/blah"
  --     bleh = enumStr . mlistForPath $ "test/bleh"
  -- MList.mforM_ (liftA2 (++) blah bleh) print
  -- return ()

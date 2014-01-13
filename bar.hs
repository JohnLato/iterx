{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
module Bar where

import Prelude hiding ((.))
import Control.Category
import Control.Monad.Base
import IterX.Stream
import IterX.Core
import qualified Data.Iteratee as I
import Data.Vector.Unboxed as V

import Criterion
import Criterion.Main

v1 :: V.Vector Int
v1 = V.enumFromN 1 10

modV1 :: Monad m => Stream m Int Int
modV1 = maps (+1)

modV2 :: Monad m => Stream m Int Int
modV2 = maps (+1) . maps (*2)

modV3 :: Monad m => Stream m Int Int
modV3 = maps (*2) . filters even . maps (+1)


-- these all fuse nicely.  Very nicely indeed.

-------------------------------------------------------------
gen1 :: Monad m => Producer m (V.Vector Int)
gen1 = yieldList [v1,v1]

prodTest1 :: IO Int
prodTest1 = runFold count gen1

prodTest2 :: IO Int
prodTest2 = runFold (foldY count $ unfolding unfoldVec) gen1

-- this is faster than the closer variant so far
prodTest2b :: IO Int
prodTest2b = runFold (foldY count $ unfolding unfoldVec2) gen1

prodTest2c :: IO Int
prodTest2c = foldG (\a b -> return $! a+b) 0 $ mapsG V.toList gen1

-- do I want to flip the order of everything?  I'd lose profunctor, unless
-- I create my own composition operator
prodTest3 :: IO Int
prodTest3 = runFold (foldY sums
    $ maps (*2) . maps (+1) . unfolding unfoldVec2) gen1

vecTest3 :: V.Vector Int -> V.Vector Int
vecTest3 = V.map (*2) . V.map (+1)

vecTest3b :: [V.Vector Int] -> Int
vecTest3b = V.sum . V.map (*2) . V.map (+1) . V.concat


iterTest2 :: IO Int
iterTest2 = I.run =<< I.enumList [v1,v1] (I.joinI $ I.mapChunks (V.toList) I.sum)

iterTest2b :: IO Int
iterTest2b = I.run =<< I.enumList [v1,v1] (I.joinI $ I.mapChunks (\x -> (:[]) $! V.sum x) I.sum)


prodTest4 :: IO Int
prodTest4 = runFold (foldY sums
    $ maps (*2) . filters even . maps (+1) . unfolding unfoldVec2) gen1

prodTest4a :: IO Int
prodTest4a = runFold (foldY sums
    $ maps (*2) . filters even . maps (+1) . unfolding unfoldVec) gen1


prodTest4b :: IO Int
prodTest4b = runFold sums
    $ transduceY (maps (*2) . filters even . maps (+1) . unfolding unfoldVec2)
      gen1

vecTest4 :: V.Vector Int -> V.Vector Int
vecTest4 = V.map (*2) . V.filter even . V.map (+1)

vecTest4b :: [V.Vector Int] -> Int
vecTest4b = V.sum . V.map (*2) . V.filter even . V.map (+1) . V.concat

instance I.Nullable (Vector Int) where
    nullC = V.null

instance I.NullPoint (Vector Int) where
    empty = V.empty

main = defaultMain
  [ bgroup "test2"
      [ bench "unfoldClosure" (prodTest2  >>= \x -> x `seq` return ())
      , bench "unfoldLoop"    (prodTest2b >>= \x -> x `seq` return ())
      , bench "justVector"  $ whnf (V.sum . V.concat) [v1,v1]
      , bench "allProducer"   (prodTest2c >>= \x -> x `seq` return ())
      , bench "iteratee"      (iterTest2 >>= \x -> x `seq` return ())
      , bench "iteratee_b"    (iterTest2b >>= \x -> x `seq` return ())
      ]
  , bgroup "test3"
      [ bench "unfoldLoop"    (prodTest3 >>= \x -> x `seq` return ())
      , bench "justVector"  $ whnf (V.sum . vecTest3 . V.concat) [v1,v1]
      , bench "justVector b" $ whnf (vecTest3b) [v1,v1]
      ]
  , bgroup "test4"
      [ bench "unfoldLoop"    (prodTest4  >>= \x -> x `seq` return ())
      , bench "unfoldClosure" (prodTest4a >>= \x -> x `seq` return ())
      , bench "transduce"     (prodTest4b >>= \x -> x `seq` return ())
      , bench "justVector"  $ whnf (V.sum . vecTest4 . V.concat) [v1,v1]
      , bench "justVector b" $ whnf (vecTest4b) [v1,v1]
      ]
  ]

-- toElems :: Monad m => TransduceFold m (V.Vector Int) Int
-- toElems = byUnfold unfoldVec

{-
summer :: Monad m => FoldM m Int Int
summer = FoldM (\a b -> return $! a+b) 0 return

prodTest2 :: IO Int
prodTest2 = runFold gen1 $ toElems $ summer

prodTest3 :: IO Int
prodTest3 = runFold gen1 $ toElems $ byStream modV3 $ byStream modV1 $ summer
-}

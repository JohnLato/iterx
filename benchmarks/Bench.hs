{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Prelude hiding ((.))
import Control.Category
import Control.Monad.Base
import IterX.Fusion
import IterX.Core
import IterX.Parser.Binary
import IterX.IterX
import Control.Applicative
import Data.Profunctor
import Data.Word
import qualified Data.Iteratee as I
import Data.ListLike.Vector
import Data.Vector.Unboxed as V

import Criterion
import Criterion.Main

v1 :: V.Vector Int
v1 = V.enumFromN 1 10

-------------------------------------------------------------
gen1 :: Monad m => Producer m (V.Vector Int)
gen1 = yieldList [v1,v1]

gen2 :: Monad m => Producer m (V.Vector Int)
gen2 = yieldList $ Prelude.replicate 100 v1

prodTest1 :: IO Int
prodTest1 = runFold count gen1

prodTest2c :: IO Int
prodTest2c = foldG (\a b -> return $! a+b) 0 $ mapsG V.toList gen1

-- do I want to flip the order of everything?  I'd lose profunctor, unless
-- I create my own composition operator
{-
prodTest3 :: IO Int
prodTest3 = runFold (foldY sums
    $ maps (*2) . maps (+1) . unfolding unfoldVec) gen1
    -}

vecTest3 :: V.Vector Int -> V.Vector Int
vecTest3 = V.map (*2) . V.map (+1)

vecTest3b :: [V.Vector Int] -> Int
vecTest3b = V.sum . V.map (*2) . V.map (+1) . V.concat


iterTest2 :: IO Int
iterTest2 = I.run =<< I.enumList [v1,v1] (I.joinI $ I.mapChunks (V.toList) I.sum)

iterTest2b :: IO Int
iterTest2b = I.run =<< I.enumList [v1,v1] (I.joinI $ I.mapChunks (\x -> (:[]) $! V.sum x) I.sum)


-- ------------------------------------------

{-
prodTest5 :: IO Int
prodTest5 = runFold (foldY (count :: FoldM IO (V.Vector Int) Int)
    $ groupVec 2 . filters even . maps (+1) . unfolding unfoldVec) gen1

prodTest5b :: IO Int
prodTest5b = runFold (foldY (count :: FoldM IO (V.Vector Int) Int)
    $ groupVec2 2 . filters even . maps (+1) . unfolding unfoldVec) gen1
-}

-- ------------------------------------------

vecTest4 :: V.Vector Int -> V.Vector Int
vecTest4 = V.map (*2) . V.filter even . V.map (+1)

vecTest4b :: [V.Vector Int] -> Int
vecTest4b = V.sum . V.map (*2) . V.filter even . V.map (+1) . V.concat

instance (Unbox a) => I.Nullable (Vector a) where
    nullC = V.null

instance (Unbox a) => I.NullPoint (Vector a) where
    empty = V.empty

--------------------------------------------------------------
-- bind-like tests

p1 :: Monad m => IterX (V.Vector Word8) m Int
p1 = fromIntegral <$> getWord16le

testVec :: V.Vector Word8
testVec = V.fromList [1..100]

numVecs :: Int
numVecs = 20

bGen :: Monad m => Producer m (V.Vector Word8)
bGen = yieldList $ Prelude.replicate numVecs testVec

pureVecTest :: V.Vector Word8 -> Int
pureVecTest = V.sum . V.map fromIntegral . V.concat . Prelude.replicate numVecs

-- a full fold, but the type is insane
-- also, multiple mapG's don't fuse, so I manually combined them
-- (and the result still isn't as good, although since delimitG is more
--  powerful than initStream it isn't quite fair yet)
genBind1 = foldG (\a b -> return $! a+b) 0 . mapG (V.sum . V.map fromIntegral . snd) . delimitG p1 (\s v -> (Left s, [(s,v)]))

iterBind :: I.Iteratee (V.Vector Word8) IO Int
iterBind = do
    s <- I.endianRead2 I.LSB
    I.joinI $ I.mapChunks (\v -> V.zip (V.replicate (V.length v) s) v)
              I.><> I.mapChunks (snd . V.unzip)
              I.><> I.mapChunks (V.map fromIntegral)
              $ I.sum

genBindTest :: IO Int
genBindTest = genBind1 bGen

iterBindTest :: IO Int
iterBindTest = I.run =<< I.enumList (Prelude.replicate numVecs testVec) iterBind

--------------------------------------------------------------
-- using just FoldM

foldTest2 :: IO Int
foldTest2 = runFold (foldUnfolding unfoldVec count) gen1

foldTest3 :: IO Int
foldTest3 = runFold (foldUnfolding unfoldVec . lmap (+1) $ lmap (*2) sums) gen1

foldTest4 :: IO Int
foldTest4 = runFold (foldUnfolding unfoldVec . lmap (+1) . filtering even $ lmap (*2) sums) gen1

foldTest5 :: IO Int
foldTest5 = runFold (foldUnfolding unfoldVec $ lmap (+1) $ filtering even $ foldVec 2 (count :: FoldM IO (V.Vector Int) Int)) gen1

foldBind1 =   runFold (initFold p1 ( \st -> lmap (st,) . lmap snd . foldUnfolding unfoldVec $ lmap fromIntegral sums) 0)

foldBindTest :: IO Int
foldBindTest = foldBind1 bGen
--------------------------------------------------------------

main = defaultMain
  [ bgroup "test2"
      [ bench "foldM"         (foldTest2 >>= \x -> x `seq` return ())
      , bench "justVector"  $ whnf (V.sum . V.concat) [v1,v1]
      , bench "allProducer"   (prodTest2c >>= \x -> x `seq` return ())
      , bench "iteratee"      (iterTest2 >>= \x -> x `seq` return ())
      , bench "iteratee_b"    (iterTest2b >>= \x -> x `seq` return ())
      ]
  , bgroup "test3"
      [ bench "foldM"         (foldTest3 >>= \x -> x `seq` return ())
      , bench "justVector"  $ whnf (V.sum . vecTest3 . V.concat) [v1,v1]
      , bench "justVector b" $ whnf (vecTest3b) [v1,v1]
      ]
  , bgroup "test4"
      [ bench "foldM"         (foldTest4 >>= \x -> x `seq` return ())
      , bench "justVector"  $ whnf (V.sum . vecTest4 . V.concat) [v1,v1]
      , bench "justVector b" $ whnf (vecTest4b) [v1,v1]
      ]
  , bgroup "test5"
      [ bench "foldM"         (foldTest5 >>= \x -> x `seq` return ())
      ]
  , bgroup "binds"
      [ bench "generator" (genBindTest >>= \x -> x `seq` return ())
      , bench "foldM"     (foldBindTest >>= \x -> x `seq` return ())
      , bench "pure vector" $ whnf (pureVecTest) (testVec)
      -- , bench "iteratee"  (iterBindTest >>= \x -> x `seq` return ())
      ]
  ]

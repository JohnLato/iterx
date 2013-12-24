{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import IterX
import System.Environment
import Control.Monad
import qualified Data.ByteString as B

twoIter :: Monad m => Int -> B.ByteString -> m Int
twoIter !s b = return $! s+B.length b

flipper = streamG f False
  where
    f False e = (True,[e])
    f True _  = (False,[])

t1 :: (Monad m) => Transducer (GenT Int (GenT Int (GenT Int m))) m Int Int
t1 = mapG (+3) . mapG (+1) . mapG (*2)
-- t1 :: (Monad m) => Transducer (GenT Int (GenT Int m)) m Int Int
-- t1 = mapG (+1) . mapG (*2)

main = do
  x <- foldG (\s e -> return $! s+e) 0 $ t1 $ yieldList [1..10]
  print x
  -- f:_ <- getArgs
  -- words <- foldG (\(!x) n -> return $! x+n) 0 (filterG even . mapG B.length $ flipper $ yieldFileChunks f 65536)
  -- print words

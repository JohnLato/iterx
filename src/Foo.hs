{-# LANGUAGE BangPatterns #-}
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

main = do
  f:_ <- getArgs
  words <- foldG (\(!x) n -> return $! x+n) 0 (filterG even . mapG B.length $ flipper $ yieldFileChunks f 65536)
  print words

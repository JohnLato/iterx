{-# LANGUAGE FlexibleContexts          #-}

{-# OPTIONS -Wall #-}
module IterX.Generators (
    yieldFileChunks
   ,yieldFileChunksHandle
) where

import           IterX.Core
import           IterX.ReadableChunk
import           IterX.Exception

import           Control.Monad

import           System.IO
import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.Trans
import           Control.Monad.Trans.Control

yieldFileChunks :: (MonadBaseControl IO m, ReadableChunk s)
                => FilePath -> Int -> Producer m s
yieldFileChunks = yieldFileChunksHandle terminateEarlyHandler

yieldFileChunksHandle :: (MonadBaseControl IO m, ReadableChunk s, Exception e)
                => (e -> m Bool) -> FilePath -> Int -> Producer m s
yieldFileChunksHandle handler fp bsize = bracket
    (liftBase $ openBinaryFile fp ReadMode)
    (liftBase . hClose)
    (\h ->
      let fillFn p = hGetBuf h p . fromIntegral
          loop = do
              SizedS n s <- liftBase $ fillFromCallback bsize fillFn
              when (n>0) $
                  try (yield s) >>=
                  either (lift . handler >=> flip when loop) (const loop)
      in loop)

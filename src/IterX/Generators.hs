{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

{-# OPTIONS -Wall #-}
module IterX.Generators (
    ExIO,
    yieldFileChunks,
    yieldFileChunksHandle,
) where

import           IterX.Core
import           IterX.ReadableChunk
import           IterX.Exception

import           Control.Monad

import           System.IO
import           Control.Monad.Trans
import           Control.Monad.Catch

#if MIN_VERSION_exceptions(0,6,0)
type ExIO m = (MonadMask m, MonadIO m)
#else
type ExIO m = (MonadCatch m, MonadIO m)
#endif

{-# INLINE yieldFileChunks #-}
yieldFileChunks :: (ExIO m, ReadableChunk s)
                => FilePath -> Int -> Producer m s
yieldFileChunks = yieldFileChunksHandle terminateEarlyHandler

{-# INLINE [1] yieldFileChunksHandle #-}
yieldFileChunksHandle :: (ExIO m, ReadableChunk s, Exception e)
                => (e -> m Bool) -> FilePath -> Int -> Producer m s
yieldFileChunksHandle handler fp bsize = bracket
    (liftIO $ openBinaryFile fp ReadMode)
    (liftIO . hClose)
    (\h ->
      let fillFn p = hGetBuf h p . fromIntegral
          loop = do
              SizedS n s <- liftIO $ fillFromCallback bsize fillFn
              when (n>0) $
                  try (yield s) >>=
                  either (lift . handler >=> flip when loop) (const loop)
      in loop)

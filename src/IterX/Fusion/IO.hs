{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.IO (
    ExIO,
    foldFile,
) where

import           IterX.ReadableChunk
import           IterX.Exception
import           IterX.Fusion.Fold
import           IterX.Generators

import           System.IO
import           Control.Monad.Trans
import           Control.Monad.Catch

{-# INLINE [1] foldFile #-}
foldFile :: (ExIO m, ReadableChunk s)
         => FilePath -> Int -> FoldM m s b -> m (FoldM m s b)
foldFile fp bsize (FoldM f s0 mkOut) = bracket
    (liftIO $ openBinaryFile fp ReadMode)
    (liftIO . hClose)
    (\h ->
      let fillFn p = hGetBuf h p . fromIntegral
          loop fState = do
              SizedS n s <- liftIO $ fillFromCallback bsize fillFn
              if (n>0)
                then
                  try (f fState s) >>= \case
                      Right fState' -> loop fState'
                      Left (TerminateEarly _) -> return (FoldM f fState mkOut)
                else return (FoldM f fState mkOut)
      in loop s0)

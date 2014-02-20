{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Transforms2 (
tdelimitN,
delimitFold4,
transduceFold,
) where

import IterX.Core
import IterX.Exception
import IterX.Fusion.Fold
import IterX.Fusion.Transforms
import IterX.IterX
import IterX.Unsafe
import qualified IterX.Parser as IX
import Data.Monoid
import Data.MonoTraversable
import Data.Sequences
import Data.Sequences (IsSequence)
import qualified Data.Sequences as S

import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Catch as E

{-# INLINE [1] tdelimitN #-}
tdelimitN :: (MonoFoldableMonoid inp, IsSequence inp, Index inp ~ Int, E.MonadCatch m)
          => IterX inp m Int
          -> Transform' m inp inp
tdelimitN i (FoldM ff fs0 fOut) = FoldM loop2 s0 mkOut
  where
    {-# INLINE [0] loop2 #-}
    loop2 s a = s a >>= \case
        MoreX k -> return k
        r@(DoneX{}) -> return $ \_ -> return r
        FailX _ err -> E.throwM $ IterFailure err
    {-# INLINE [0] mkOut #-}
    mkOut s = s mempty >>= \case
        DoneX fs' _ -> fOut fs'
        FailX _ err -> E.throwM $ IterFailure err
        MoreX k     -> fOut fs0
    s0 s = runIter (i >>= \n -> IX.foldI n ff fs0) s HasMore failX doneX

{-# INLINE [1] loopIter #-}
loopIter :: (E.MonadCatch m)
         => IterX s m a -> FoldM m a b -> FoldM m s b
loopIter iter (FoldM ff fs0 fOut) =
    FoldM loop (s0,fs0) (fOut.snd)
  where
    {-# INLINE [0] loop #-}
    loop (s,fs) a = s a >>= \case
        MoreX k -> return (k,fs)
        DoneX r rest -> do
            fs' <- ff fs r
            loop2 (s0,fs') rest
        FailX _ err  -> E.throwM $ IterFailure err
    loop2 (s,fs) a = s a >>= \case
        MoreX k -> return (k,fs)
        DoneX r rest -> do
            fs' <- ff fs r
            loop2 (s0,fs') rest
        FailX _ err  -> E.throwM $ IterFailure err
    s0 = \s -> runIter iter s HasMore failX doneX

{-# INLINE delimitFold4 #-}
delimitFold4 :: (E.MonadCatch m, Int ~ Index i, MonoFoldableMonoid i
                , IsSequence i)
             => IterX i m (Int,st)
             -> (st -> FoldM m i o)
             -> FoldM m o o2
             -> FoldM m i o2
delimitFold4 iter selFold outfold = loopIter mainIter outfold
  where
    {-# INLINE mainIter #-}
    mainIter = do
        (n,st) <- iter
        case selFold st of
            FoldM ff fs0 fOut -> IX.foldI n ff fs0 >>= lift . fOut
    
{-
{-# INLINE tdelimitN #-}
tdelimitN iter f = delimitFold3 iter f1 f2 f3
  where
    f1 = const f
    f2 n = foldIterLeftover (IX.splitChunkAt n)
    f3 = foldLast (error "<iterx> tdelimitN: no outputs at all!")
    -}

transduceFold :: Monad m
              => Transform' m i o
              -> (forall s. Producer (StateT s m) i)
              -> Producer m o
transduceFold t gen = do
    sink <- ask
    let fold = t $ foldingM (\_ o -> sink o) ()
    lift $ runFold_ fold gen
{-# INLINE transduceFold #-}

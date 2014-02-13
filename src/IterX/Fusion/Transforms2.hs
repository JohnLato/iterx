{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Transforms2 (
tdelimitN,
transduceFold,
) where

import IterX.Core
import IterX.Fusion.Fold
import IterX.Fusion.Transforms
import IterX.IterX
import qualified IterX.Parser as IX
import Data.MonoTraversable
import Data.Sequences

import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.Catch as E

{-# INLINE tdelimitN #-}
tdelimitN :: (MonoFoldableMonoid inp, IsSequence inp, Index inp ~ Int, E.MonadCatch m)
          => IterX inp m Int
          -> Transform' m inp inp
tdelimitN iter f = delimitFold3 iter f1 f2 f3
  where
    f1 = const f
    f2 n = foldIterLeftover (IX.splitChunkAt n)
    f3 = foldLast (error "<iterx> tdelimitN: no outputs at all!")

transduceFold :: Monad m
              => Transform' m i o
              -> (forall s. Producer (StateT s m) i)
              -> Producer m o
transduceFold t gen = do
    sink <- ask
    let fold = t $ foldingM (\_ o -> sink o) ()
    lift $ runFold_ fold gen
{-# INLINE transduceFold #-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Transforms2 (
tdelimitN,
) where

import IterX.Fusion.Fold
import IterX.Fusion.Transforms
import IterX.IterX
import qualified IterX.Parser as IX
import Data.MonoTraversable
import Data.Sequences

{-# INLINE [1] tdelimitN #-}
tdelimitN :: (MonoFoldableMonoid inp, IsSequence inp, Index inp ~ Int, Monad m)
          => IterX inp m Int
          -> Transform' m inp inp
tdelimitN iter f = delimitFold3 iter f1 f2 f3
  where
    f1 = const f
    f2 n = foldIterLeftover (IX.splitChunkAt n)
    f3 = foldLast (error "<iterx> tdelimitN: no outputs at all!")

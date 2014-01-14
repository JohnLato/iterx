{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Fold where

import Prelude hiding (id, (.))
import qualified Prelude as P
import IterX.Core
import IterX.Fusion.Unfold

import Control.Category
import Data.Profunctor
import Control.Monad
import Control.Monad.State

--------------------------------------------------------

-- A Fold is like a Stream, but there are no intermediate output values.
-- There's also no indication of termination.
--
-- FoldM is strictly stronger than stream, because it also has an initial
-- value.
data FoldM m a b where
  FoldM :: (s -> a -> m s) -> s -> (s -> m b) -> FoldM m a b

instance Monad m => Profunctor (FoldM m) where
    {-# INLINE dimap #-}
    dimap = dimapFold

{-# INLINE [1] dimapFold #-}
dimapFold :: Monad m => (a->b) -> (c->d) -> FoldM m b c -> FoldM m a d
dimapFold lf rf (FoldM loop s0 mkOut) = FoldM loop' s0 (liftM rf . mkOut)
  where
    {-# INLINE [0] loop' #-}
    loop' s a = loop s $ lf a

-- -----------------------------------------

{-# INLINE [0] runFold #-}
runFold :: Monad m => FoldM m i o -> (forall s. Producer (StateT s m) i) -> m o
runFold (FoldM f s0 mkOut) gen = foldG f s0 gen >>= mkOut

{-# INLINE [0] runFold_ #-}
runFold_ :: Monad m => FoldM m i o ->  (forall s. Producer (StateT s m) i) -> m ()
runFold_ (FoldM f s0 _mkOut) gen = const () `liftM` foldG f s0 gen

-- -----------------------------------------

{-# INLINE [1] foldUnfolding #-}
foldUnfolding :: Monad m => UnfoldM m a b -> FoldM m b c -> FoldM m a c
foldUnfolding (UnfoldM mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\s a -> loop2 (mkUnf a) s) s0 mkOut
  where
    -- it's much faster to leave this un-INLINEd for simple tests,
    -- but on prodTest4, it makes the regular vector unfolding
    -- more efficient.  Need more data to know what's best.
    loop2 unfState foldState = uf unfState >>= \case
        Just (a, unfState') -> f foldState a >>= loop2 unfState'
        Nothing -> return foldState
foldUnfolding (SUnfoldM unfS0 mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\(unfS,s) a -> loop2 (mkUnf unfS a) s) (unfS0,s0) (mkOut.snd)
  where
    -- it's much faster to leave this un-INLINEd for simple tests,
    -- but on prodTest4, it makes the regular vector unfolding
    -- more efficient.  Need more data to know what's best.
    loop2 unfState foldState = uf unfState >>= \case
        Right (a, unfState') -> f foldState a >>= loop2 unfState'
        Left unfState' -> return (unfState',foldState)

{-# RULES "<iterx> fold/unfoldId" forall f. foldUnfolding unfoldIdM f = f #-}

--------------------------------------------------------

class Folding p where
    liftFold :: Monad m => FoldM m i o -> p m i o

instance Folding FoldM where
    liftFold = P.id

--------------------------------------------------------

-- perform a *strict* fold
{-# INLINE folding #-}
folding :: (Folding p, Monad m) => (b -> a -> b) -> b -> p m a b
folding f s0 = foldingM (\b a -> return $! f b a) s0

{-# INLINE foldingM #-}
foldingM :: (Folding p, Monad m) => (b -> a -> m b) -> b -> p m a b
foldingM  f s0 = liftFold $ FoldM f s0 return

{-# INLINE sums #-}
sums :: (Folding p, Num a, Monad m) => p m a a
sums = folding (+) 0

{-# INLINE products #-}
products :: (Folding p, Num a, Monad m) => p m a a
products = folding (*) 1

{-# INLINE count #-}
count :: (Folding p, Num cnt, Monad m) => p m a cnt
count = folding (\b _ -> b+1) 0

{-# INLINE zippingWith #-}
zippingWith
  :: (Folding p, Monad m)
  => (b -> c -> d)
  -> FoldM m a b
  -> FoldM m a c
  -> p m a d
zippingWith cmb (FoldM f1 s1_0 out1) (FoldM f2 s2_0 out2) =
    liftFold $ FoldM loop (s1_0,s2_0) mkOut
  where
    {-# INLINE [0] loop #-}
    loop (s1,s2) a = liftM2 (,) (f1 s1 a) (f2 s2 a)
    {-# INLINE [0] mkOut #-}
    mkOut (s1,s2) = liftM2 cmb (out1 s1) (out2 s2)

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Transforms (
Transform,
Transform',

maps,
mapsM,
filters,
filterMaybe,
scans,
mealy,
mealyM,

cmap,
cmap',
foldUnfolding,

) where

import IterX.Fusion.Fold
import IterX.Fusion.Unfold
import Data.Profunctor
import Data.Maybe as Maybe
import Control.Monad
import qualified Data.Vector.Generic as G

import GHC.Exts

type Transform m1 m2 a b = forall c. FoldM m1 b c -> FoldM m2 a c
type Transform' m a b = Transform m m a b

maps :: Monad m => (a -> b) -> Transform' m a b
maps f = lmap f

{-# INLINE [1] mapsM #-}
mapsM :: Monad m => (a -> m b) -> Transform' m a b
mapsM f (FoldM ff s0 mkOut) = FoldM loop s0 mkOut
  where
    {-# INLINE [0] loop #-}
    loop s a = f a >>= ff s

{-# INLINE [1] filters #-}
filters :: Monad m => (a->Bool) -> Transform' m a a
filters p (FoldM f s0 mkOut) = FoldM f' s0 mkOut
  where
    {-# INLINE [0] f' #-}
    f' s a | p a = f s a
           | otherwise = return s

{-# INLINE filterMaybe #-}
filterMaybe :: Monad m => Transform' m (Maybe a) a
filterMaybe = filters Maybe.isJust . maps fromJust

{-# INLINE [1] scans #-}
scans :: Monad m => FoldM m a b -> Transform' m a b
scans (FoldM scf scs0 scOut) (FoldM ff s0 fOut) =
    FoldM f' (scs0,s0) (fOut . snd)
  where
    {-# INLINE [0] f' #-}
    f' (scs,fs) a = do
        scs' <- scf scs a
        fs' <- ff fs =<< scOut scs'
        return (scs',fs')

{-# INLINE [1] mealyM #-}
mealyM :: Monad m => (s -> a -> m (s,[b])) -> s -> Transform' m a b
mealyM f s0 (FoldM ff fs0 fOut) = FoldM loop (s0,fs0) (fOut . snd)
  where
    {-# INLINE [0] loop #-}
    loop (s,fs) a = do
        (!s',bs) <- f s a
        fs' <- foldM ff fs bs
        return (s',fs')

{-# INLINE mealy #-}
mealy :: Monad m => (s -> a -> (s,[b])) -> s -> Transform' m a b
mealy f s0 = mealyM (\s a -> return $ f s a) s0

{-# INLINE cmap #-}
cmap :: Monad m => (a -> [b]) -> Transform' m a b
cmap f = maps f . foldUnfolding unfoldList

{-# INLINE cmap' #-}
cmap' :: forall m a b. Monad m => (a -> UnfoldM m () b) -> Transform' m a b
cmap' f (FoldM ff s0 mkOut) = FoldM f' s0 mkOut
  where
    {-# INLINE f' #-}
    f' s a = case f a of
        (UnfoldM mkUnf uf) -> mkUnf () >>= \uf' -> loop uf uf' s
    {-# INLINE [0] loop #-}
    loop uf ufS = \fs -> uf ufS >>= \case
        Just (b,ufS') -> ff fs b >>= loop uf ufS'
        Nothing       -> return fs

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}

-- Fold over an unfolding.
{-# INLINE foldUnfolding #-}
foldUnfolding :: Monad m => UnfoldM m a b -> FoldM m b c -> FoldM m a c
foldUnfolding (UnfoldM mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\s a -> mkUnf a >>= uf >>= loop2 SPEC s) s0 mkOut
  where
    loop2 !sPEC foldState unfState = case unfState of
        Just !(!a, unfState') -> do
            fs' <- f foldState a
            us' <- uf unfState'
            loop2 SPEC fs' us'
        Nothing -> return foldState
foldUnfolding (SUnfoldM unfS0 mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\(unfS,s) a -> loop2 SPEC (mkUnf unfS a) s) (unfS0,s0) (mkOut.snd)
  where
    loop2 !sPEC unfState foldState = uf unfState >>= \case
        Right (a, unfState') -> f foldState a >>= loop2 SPEC unfState'
        Left unfState' -> return (unfState',foldState)

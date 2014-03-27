{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
umealy,
umealyM,

cmap,
cmap',
foldUnfolding,
foldUnfolding2,
foldUnfolding2a,
unfolding,
unfoldingM,

liftInner,
foldU',
SPEC(..),
) where

import IterX.Fusion.Fold
import IterX.Fusion.Unfold
import Data.Profunctor
import Data.Maybe as Maybe
import Control.Monad
import Data.Functor.Identity
import qualified Data.Vector.Generic as G

import GHC.Exts

type Transform m1 m2 a b = forall c. FoldM m1 b c -> FoldM m2 a c
type Transform' m a b = Transform m m a b

maps :: Monad m => (a -> b) -> Transform' m a b
maps f = lmap f

{-# RULES
"<iterx>maps/maps" forall f g. maps f . maps g = maps (g . f)
"<iterx>lmap/lmap" forall f g. lmap f . lmap g = lmap (g . f)
"<iterx>unfold2/maps" forall f uf. foldUnfolding2 uf . maps f = foldUnfolding2 ((fmap . fmap) f uf)
"<iterx>maps/unfold2" forall f uf. maps f . foldUnfolding2 uf = foldUnfolding2 (uf . f)
"<iterx>maps/unfolding" forall f g uf. maps f (unfolding uf g) = unfolding (uf . f) g
"<iterx>lower unfoldingM" forall u f. unfoldingM u f = unfolding u f
     #-}

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

{-# INLINE umealy #-}
umealy :: Monad m => (s -> a -> (s,UnfoldM m () b)) -> s -> Transform' m a b
umealy f s0 = umealyM (\s a -> return $ f s a) s0

{-# INLINE [1] umealyM #-}
umealyM :: Monad m => (s -> a -> m (s,UnfoldM m () b)) -> s -> Transform' m a b
umealyM f s0 (FoldM ff fs0 fOut) = FoldM loop (s0,fs0) (fOut . snd)
  where
    {-# INLINE [0] loop #-}
    loop (s,fs) a = do
        (!s',UnfoldM mkUnf uf) <- f s a
        uf0 <- mkUnf ()
        uf1 <- uf uf0
        let loop2 !sPEC foldState unfState = case unfState of
                UnfoldStep a unfState' -> do
                    fs' <- ff foldState a
                    us' <- uf unfState'
                    loop2 SPEC fs' us'
                UnfoldDone -> return foldState
        fs' <- loop2 SPEC fs uf1
        return (s',fs')

{-# INLINE [1] mealyM #-}
mealyM :: Monad m => (s -> a -> m (s,b)) -> s -> Transform' m a b
mealyM f s0 (FoldM ff fs0 fOut) = FoldM loop (s0,fs0) (fOut . snd)
  where
    {-# INLINE [0] loop #-}
    loop (s,fs) a = do
        (!s',bs) <- f s a
        fs' <- ff fs bs
        return (s',fs')

{-# INLINE mealy #-}
mealy :: Monad m => (s -> a -> (s,b)) -> s -> Transform' m a b
mealy f s0 = mealyM (\s a -> return (f s a)) s0

{-# INLINE cmap #-}
cmap :: Monad m => (a -> [b]) -> Transform' m a b
cmap f = maps f . foldUnfolding unfoldList

{-# INLINE cmap' #-}
cmap' :: forall m a b. Monad m => (a -> UnfoldM m () b) -> Transform' m a b
cmap' f (FoldM ff s0 mkOut) = FoldM f' s0 mkOut
  where
    {-# INLINE f' #-}
    f' s a = case f a of
        (UnfoldM mkUnf uf) -> mkUnf () >>= \uf' -> loop SPEC uf uf' s
    loop !sPEC uf ufS = \fs -> uf ufS >>= \case
        UnfoldStep b ufS' -> ff fs b >>= loop SPEC uf ufS'
        UnfoldDone        -> return fs

data SPEC = SPEC | SPEC2
{-# ANN type SPEC ForceSpecConstr #-}

-- Fold over an unfolding.
{-# INLINE [1] foldUnfolding #-}
foldUnfolding :: Monad m => UnfoldM m a b -> FoldM m b c -> FoldM m a c
foldUnfolding (UnfoldM mkUnf uf) (FoldM f s0 mkOut) =
    FoldM loop1 s0 mkOut
  where
    loop1 s a = do
        ufs <- mkUnf a
        ufs' <- uf ufs
        loop2 SPEC s ufs'
    loop2 !sPEC foldState unfState = case unfState of
        UnfoldStep a unfState' -> do
            fs' <- f foldState a
            us' <- uf unfState'
            loop2 SPEC fs' us'
        UnfoldDone -> return foldState

{-# INLINE [1] liftInner #-}
liftInner :: (forall x. g x -> h x) -> FoldM g a b -> FoldM h a b
liftInner mX (FoldM f s0 mkOut) =
    FoldM (\s a -> mX $ f s a) s0 (mX . mkOut)

{-# INLINE [1] unfolding #-}
unfolding :: Monad m => (a -> Unfold2 Identity b) -> FoldM Identity b c -> FoldM m a c
unfolding unf (FoldM f s0 mkOut) = rmapFoldM mkOut' $ folding (\s v -> runIdentity $ stepFold (foldUnfolding2 unf (foldingM f s)) v >>= getFold) s0
  where
    mkOut' = return . runIdentity . mkOut

{-# INLINE [1] unfoldingM #-}
unfoldingM :: Monad m => (a -> Unfold2 m b) -> FoldM m b c -> FoldM m a c
unfoldingM unf (FoldM f s0 mkOut) = rmapFoldM mkOut $ foldingM (\s v -> stepFold (foldUnfolding2 unf (foldingM f s)) v >>= getFold) s0


-- Fold over an unfolding.
{-# INLINE [1] foldUnfolding2 #-}
foldUnfolding2 :: Monad m => (a -> Unfold2 m b) -> FoldM m b c -> FoldM m a c
foldUnfolding2 mkUnf (FoldM f s0 mkOut) =
    FoldM loop1 s0 mkOut
  where
    loop1 s a = case mkUnf a of
        (Unfold2 ufs0 uf) -> do
            ufs <- uf ufs0
            let loop2 !sPEC foldState unfState = case unfState of
                        UnfoldStep a unfState' -> do
                            fs' <- f foldState a
                            us' <- uf unfState'
                            loop2 SPEC fs' us'
                        UnfoldDone -> return foldState
            loop2 SPEC s ufs

-- Fold over an unfolding.
{-# INLINE foldUnfolding2a #-}
foldUnfolding2a :: Monad m => FoldM m a c -> FoldM m (Unfold2 m a) c
foldUnfolding2a (FoldM f s0 mkOut) =
    FoldM (foldU' f) s0 mkOut

{-# INLINE [1] foldU' #-}
foldU' :: Monad m => (s -> a -> m s) -> s -> Unfold2 m a -> m s
foldU' fFunc fState0 (Unfold2 unfState0 uf) = loop SPEC fState0 unfState0
  where
    loop !sPEC fState unfState = do
        r <- uf unfState
        case r of
            UnfoldStep a unfState' -> do
                fs' <- fFunc fState a
                loop SPEC fs' unfState'
            UnfoldDone -> return fState

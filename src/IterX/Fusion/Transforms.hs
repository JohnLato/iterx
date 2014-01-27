{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Transforms (
maps,
mapsM,
filters,
filterMaybe,
scans,

cmap,
cmap',
foldCmap,
foldUnfolding,
) where

import IterX.Fusion.Fold
import IterX.Fusion.Unfold
import Data.Profunctor
import Data.Maybe as Maybe
import GHC.Exts (build)

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

{-# INLINE cmap #-}
cmap :: Monad m => (a -> [b]) -> Transform' m a b
cmap f = maps f . foldUnfolding unfoldList

{-# INLINE cmap' #-}
cmap' :: forall m a b. Monad m => (a -> UnfoldM m () b) -> Transform' m a b
cmap' f (FoldM ff s0 mkOut) = FoldM f' s0 mkOut
  where
    {-# INLINE f' #-}
    f' s a = case f a of
        (UnfoldM mkUnf uf) -> loop uf (mkUnf ()) s
    {-# INLINE [0] loop #-}
    loop uf = \ufS fs -> uf ufS >>= \case
        Just (b,ufS') -> ff fs b >>= loop uf ufS'
        Nothing       -> return fs

-- Fold over an unfolding.
{-# INLINE [1] foldUnfolding #-}
foldUnfolding :: Monad m => UnfoldM m a b -> FoldM m b c -> FoldM m a c
foldUnfolding (UnfoldM mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\s a -> loop2 (mkUnf a) s) s0 mkOut
  where
    -- INLINE-ing this is a big loss.  For Streams.  Maybe no longer
    -- true with Folds?
    loop2 unfState foldState = uf unfState >>= \case
        Just (a, unfState') -> f foldState a >>= loop2 unfState'
        Nothing -> return foldState
foldUnfolding (SUnfoldM unfS0 mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\(unfS,s) a -> loop2 (mkUnf unfS a) s) (unfS0,s0) (mkOut.snd)
  where
    loop2 unfState foldState = uf unfState >>= \case
        Right (a, unfState') -> f foldState a >>= loop2 unfState'
        Left unfState' -> return (unfState',foldState)

-- this is a lot of extra work to attempt to work well with foldr/build
-- fusion.  And the performance still isn't as good as the plain code.
--
{-# INLINE foldCmap #-}
foldCmap :: Monad m => (a -> [b]) -> Transform' m a b
foldCmap f (FoldM ff s0 mkOut) = FoldM (\s a -> foldLoop ff (f a) s) s0 mkOut
  -- where
    -- {-# INLINE inner #-}
    -- inner s a = foldLoop ff (f a) s

{-# INLINE [1] foldLoop #-}
foldLoop :: Monad m => (s -> b -> m s) -> [b] -> s -> m s
foldLoop ff = go
  where
    go []     foldState = return foldState
    go (b:bs) foldState = ff foldState b >>= go bs

{-# INLINE [0] foldLoop' #-}
foldLoop' :: (Monad m) => (s -> b -> m s) -> (forall x. (b->x->x) -> x -> x) -> s -> m s
foldLoop' f g0 s = g0 (\a g s -> (f s a) >>= g) return s

{-# RULES
"<iterx>cmap" forall f.  maps f . foldUnfolding unfoldList = foldCmap f
"<iterx>cmap/build" forall f (g :: forall b. (a->b->b)->b->b). foldLoop f (build g) = foldLoop' f g
      #-}

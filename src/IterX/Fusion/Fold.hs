{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Fold (
FoldM (..),
Folding (..),

-- * running folds
runFold,
runFold_,
getFold,
stepFold,

-- * creating folds
folding,
foldingM,
-- ** specific folds
toList,
sums,
products,
count,
zippingWith,

foldFirst,
foldLast,
foldConst,

foldIterLeftover,
foldFoldable,

foldVec,
initFold,
delimitFold2,
delimitFold3,
) where

import Prelude hiding (id, (.))
import qualified Prelude as P
import IterX.Core
import IterX.IterX
import IterX.Generators (ExIO)
import IterX.Exception

import Control.Category
import qualified Control.Monad.Catch as E
import Data.MonoTraversable
import Data.Profunctor
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad
import Control.Monad.State

import GHC.IO (unsafeDupablePerformIO)
import Data.Typeable
import Data.Monoid
import Unsafe.Coerce

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
    {-# INLINE lmap #-}
    lmap  = lmapFold

{-# INLINE [1] dimapFold #-}
dimapFold :: Monad m => (a->b) -> (c->d) -> FoldM m b c -> FoldM m a d
dimapFold lf rf (FoldM loop s0 mkOut) = FoldM loop' s0 (liftM rf . mkOut)
  where
    {-# INLINE [0] loop' #-}
    loop' s a = loop s $ lf a

{-# INLINE [1] lmapFold #-}
lmapFold :: Monad m => (a->b) -> FoldM m b c -> FoldM m a c
lmapFold lf (FoldM loop s0 mkOut) = FoldM loop' s0 mkOut
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

{-# INLINE [1] getFold #-}
getFold :: FoldM m i o -> m o
getFold (FoldM _ s out) = out s

-- -----------------------------------------
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

{-# INLINE foldLast #-}
foldLast :: (Folding p, Monad m) => i -> p m i i
foldLast x0 = folding (\_ x -> x) x0

{-# INLINE foldFirst #-}
foldFirst :: (Folding p, Monad m) => p m i (Maybe i)
foldFirst = liftFold $ FoldM loop Nothing return
  where
    {-# INLINE [0] loop #-}
    loop Nothing  i = return $ Just i
    loop s@Just{} _ = return s

{-# INLINE foldConst #-}
foldConst :: (Folding p, Monad m) => a -> p m i a
foldConst a = liftFold $ FoldM loop () (const $ return a)
  where
    {-# INLINE [0] loop #-}
    loop _ _ = return ()

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

{-# INLINE toList #-}
toList :: (Folding p, Monad m) => p m a [a]
toList = liftFold $ FoldM loop id (return . ($ []))
  where
    {-# INLINE [0] loop #-}
    loop acc el = return $ acc . (el:)

{-# INLINE [1] foldVec #-}
foldVec :: (MonadIO m, G.Vector v i) => Int -> FoldM m (v i) b -> FoldM m i b
foldVec n (FoldM ff fs0 fOut)
    | n > 1 = FoldM loop (unsafeDupablePerformIO (GM.unsafeNew n),0,fs0) (fOut . (\(_,_,fs) -> fs))
    | n == 1 = error "TODO: implement for 1"
    | otherwise = error $ "<iterx> foldVec: n " ++ show n
  where
    {-# INLINE [0] loop #-}
    loop (v,thisIx,fs) i
        | thisIx == n-1 = do
            (v',v'2) <- liftIO $ do
                GM.unsafeWrite v thisIx i
                v' <- G.unsafeFreeze v
                v'2 <- GM.unsafeNew n
                return (v',v'2)
            fs' <- ff fs v'
            return (v'2,0,fs')
        | otherwise = liftIO $ do
            GM.unsafeWrite v thisIx i
            return (v,thisIx+1,fs)

{-# INLINE [1] initFold #-}
initFold :: (E.MonadCatch m)
         => IterX i m st -> (st -> FoldM m i o) -> o -> FoldM m i o
initFold iter sel o0 = FoldM loop (StartDelimiter) extract
  where
    {-# INLINE extract #-}
    extract (ProcState (FoldM _ s out)) = out s
    extract (_) = return o0
    {-# INLINE loop #-}
    loop (ProcState fold) i = do
        fold' <- stepFold fold i
        return $ ProcState fold'
    loop (StartDelimiter) i = runIter iter i HasMore failX doneX >>= procRes
    loop (ConsumeDelimiter k) i = k i >>= procRes
    {-# INLINE [0] procRes #-}
    procRes res = case res of
      MoreX k' -> return $ ConsumeDelimiter k'
      DoneX s' rest -> do
          let ifold = sel s'
          fold' <- stepFold ifold rest
          return (ProcState fold')
      FailX _ err -> E.throwM $
          IterFailure $ "<iterx> initFold failure: " ++ err

stepFold :: Monad m => FoldM m i o -> i -> m (FoldM m i o)
stepFold (FoldM f s out) i = do
    s' <- f s i
    return $ FoldM f s' out

{-# INLINE [1] foldCombiner #-}
foldCombiner :: Monad m
             => FoldM m i o
             -> FoldM m i (Maybe (i,i))
             -> FoldM m i (o, Maybe i)
foldCombiner (FoldM fff ffs0 ffout) (FoldM ssf sss0 ssout) =
    FoldM loop (ffs0,sss0,Nothing) extract
  where
    {-# INLINE extract #-}
    extract (ffs,_,val) = do
        o <- ffout ffs
        return (o,val)
    {-# INLINE [0] loop #-}
    loop (ffs,sss,val) i = do
        sss' <- ssf sss i
        ssout sss' >>= \case
          Nothing -> do
            ffs' <- fff ffs i
            return (ffs',sss',val)
          Just (iThis,iNext) -> do
            ffs' <- fff ffs iThis
            return (ffs',sss',Just iNext)

-- this seems a little less efficient than exception-based
-- without much time spent optimizing either
{-# INLINE [1] delimitFold2 #-}
delimitFold2 :: ( E.MonadCatch m)
             => IterX i m st
             -> (st -> FoldM m i o)
             -> (st -> FoldM m i (Maybe (i,i)))
             -> FoldM m o o2
             -> FoldM m i o2
delimitFold2 iter selFold selStop outfold
    = FoldM loop (StartDelimiter,outfold) extract
  where
    {-# INLINE extract #-}
    extract (ProcState (FoldM _ s out,_),FoldM oF oS oOut) =
        out s >>= oF oS >>= oOut
    extract (_,FoldM _ oS oOut) = oOut oS
    {-# INLINE [0] loop #-}
    loop (ProcState (fold,sfold),ofold) i =
        doFold ofold sfold fold i
    loop (StartDelimiter,ofold) i = runIter iter i HasMore failX doneX >>= procRes ofold
    loop (ConsumeDelimiter k,ofold) i = k i >>= procRes ofold

    {-# INLINE doFold #-}
    doFold ofold (FoldM sff sfs sfout) ifold inp = do
        sfs' <- sff sfs inp
        sfout sfs' >>= \case
          Nothing -> do
              fold' <- stepFold ifold inp
              return (ProcState (fold',FoldM sff sfs' sfout),ofold)
          Just (iThis,iRest) -> do
              o <- stepFold ifold iThis >>= getFold
              ofold' <- stepFold ofold o
              res <- runIter iter iRest HasMore failX doneX
              procRes ofold' res

    {-# INLINE [1] procRes #-}
    procRes ofold res = case res of
      MoreX k' -> return $ (ConsumeDelimiter k', ofold)
      DoneX s' rest -> doFold ofold (selStop s') (selFold s') rest
      FailX _ err -> E.throwM $
          IterFailure $ "<iterx> initFold failure: " ++ err

-- this seems better than either earlier delimitFold variant, but
-- only when delimitFold2 is defined.  GHC is doing some CSE or
-- something that is causing difficulty...
{-# INLINE [1] delimitFold3 #-}
delimitFold3 :: ( E.MonadCatch m)
             => IterX i m st
             -> (st -> FoldM m i o)
             -> (st -> FoldM m i (Maybe (i,i)))
             -> FoldM m o o2
             -> FoldM m i o2
delimitFold3 iter selFold selStop outfold
    = FoldM loop (StartDelimiter,outfold) extract
  where
    {-# INLINE extract #-}
    extract (ProcState (FoldM _ s out),FoldM oF oS oOut) = do
        (o,_) <- out s
        oF oS o >>= oOut
    extract (_,FoldM _ oS oOut) = oOut oS
    {-# INLINE [0] loop #-}
    loop (ProcState fold,ofold) i =
        doFold ofold fold i
    loop (StartDelimiter,ofold) i = runIter iter i HasMore failX doneX >>= procRes ofold
    loop (ConsumeDelimiter k,ofold) i = k i >>= procRes ofold

    {-# INLINE doFold #-}
    doFold ofold (FoldM sff sfs sfout) inp = do
        sfs' <- sff sfs inp
        sfout sfs' >>= \case
          (_,Nothing) -> do
              return (ProcState (FoldM sff sfs' sfout),ofold)
          (o,Just iRest) -> do
              ofold' <- stepFold ofold o
              res <- runIter iter iRest HasMore failX doneX
              procRes ofold' res

    {-# INLINE [0] procRes #-}
    procRes ofold res = case res of
      MoreX k' -> return $ (ConsumeDelimiter k', ofold)
      DoneX s' rest ->
          let theFold = foldCombiner (selFold s') (selStop s')
          in doFold ofold theFold rest
      FailX _ err -> E.throwM $
          IterFailure $ "<iterx> initFold failure: " ++ err

{-# INLINE [1] foldIterLeftover #-}
foldIterLeftover :: E.MonadCatch m
                 => IterX i m a -> FoldM m i (Maybe (a,i))
foldIterLeftover iter = FoldM loop StartDelimiter extract
  where
    extract (ProcState p) = return $ Just p
    extract _             = return Nothing
    {-# INLINE [0] loop #-}
    loop (ConsumeDelimiter k) i = k i >>= proc
    loop StartDelimiter i =
        runIter iter i HasMore failX doneX >>= proc
    loop p@(ProcState _) _i = return p
    {-# INLINE [0] proc #-}
    proc res = case res of
      MoreX k' -> return $ ConsumeDelimiter k'
      DoneX s' rest -> return $ ProcState (s',rest)
      FailX _ err -> E.throwM $
          IterFailure $ "<iterx> initFold failure: " ++ err

{-# INLINE [1] foldFoldable #-}
foldFoldable :: (Monad m, MonoFoldable full)
             => FoldM m (Element full) b -> FoldM m full b
foldFoldable (FoldM ff fs0 fOut) = FoldM loop fs0 fOut
  where
    {-# INLINE [0] loop #-}
    loop s full = ofoldlM ff s full


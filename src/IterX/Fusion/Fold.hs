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

-- * creating folds
folding,
foldingM,
-- ** specific folds
toList,
sums,
products,
count,
zippingWith,

filtering,
foldVec,
initFold,
) where

import Prelude hiding (id, (.))
import qualified Prelude as P
import IterX.Core
import IterX.IterX
import IterX.Exception

import Control.Category
import qualified Control.Exception as E
import Data.Profunctor
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Control.Monad
import Control.Monad.Base
import Control.Monad.State

import GHC.IO (unsafeDupablePerformIO)

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

{-# INLINE [1] filtering #-}
filtering :: Monad m => (a->Bool) -> FoldM m a b -> FoldM m a b
filtering p (FoldM f s0 out) = FoldM f' s0 out
  where
    {-# INLINE [0] f' #-}
    f' s a | p a = f s a
           | otherwise = return s

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
foldVec :: (MonadBase IO m, G.Vector v i) => Int -> FoldM m (v i) b -> FoldM m i b
foldVec n (FoldM ff fs0 fOut)
    | n > 1 = FoldM loop (unsafeDupablePerformIO (GM.unsafeNew n),0,fs0) (fOut . (\(_,_,fs) -> fs))
    | n == 1 = error "TODO: implement for 1"
    | otherwise = error $ "<iterx> foldVec: n " ++ show n
  where
    {-# INLINE [0] loop #-}
    loop (v,thisIx,fs) i
        | thisIx == n-1 = do
            (v',v'2) <- liftBase $ do
                GM.unsafeWrite v thisIx i
                v' <- G.unsafeFreeze v
                v'2 <- GM.unsafeNew n
                return (v',v'2)
            fs' <- ff fs v'
            return (v'2,0,fs')
        | otherwise = liftBase $ do
            GM.unsafeWrite v thisIx i
            return (v,thisIx+1,fs)

{-# INLINE [1] initFold #-}
initFold :: (Monad m)
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
      FailX _ err -> E.throw $
          IterFailure $ "<iterx> initFold failure: " ++ err

stepFold :: Monad m => FoldM m i o -> i -> m (FoldM m i o)
stepFold (FoldM f s out) i = do
    s' <- f s i
    return $ FoldM f s' out


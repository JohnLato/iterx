{-# LANGUAGE BangPatterns              #-}

{-# OPTIONS -Wall #-}
module IterX.Core (
   GenT
  ,Producer
  ,Consumer
  ,Transducer
  
  ,runGenT
  ,yield
  ,yieldList

  ,mapG
  ,mapGM
  ,mapsG
  ,filterG
  ,foldG
  ,streamG
  ,streamGM
  ,unStateP

  ,indexG
  ,dropG
) where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.Foldable as Fold

type GenT e m = ReaderT (e -> m ()) m
type Producer m e = GenT e m ()
type Consumer m e = e -> m ()
type Transducer m1 m2 e1 e2 = Producer m1 e1 -> Producer m2 e2

runGenT :: Monad m => Producer m e -> Consumer m e -> m ()
runGenT = runReaderT

yield :: Monad m => e -> Producer m e
yield e = ask >>= lift . ($ e)
{-# INLINE yield #-}

yieldList :: Monad m => [e] -> Producer m e
yieldList = mapM_ yield
{-# INLINE yieldList #-}

-----------------------------------------------------------------
-- fusion stuff

-- this is actually used....
unMP :: (Monad m, MonadTrans t)
     => ((t m) () -> m ())
     -> Producer (t m) e
     -> Producer m e
unMP unP p = do
    env <- ask
    lift $ unP (runReaderT p (\e -> lift $ env e))
{-# INLINE unMP #-}

-----------------------------------------------------------------
-- Transducers

mapG :: Monad m => (e1 -> e2) -> Transducer (GenT e2 m) m e1 e2
mapG f gen = runGenT gen (yield . f)
{-# INLINE mapG #-}

mapsG :: Monad m => (e1 -> [e2]) -> Transducer (GenT e2 m) m e1 e2
mapsG f gen = runGenT gen (mapM_ yield . f)

mapGM :: Monad m => (e1 -> m e2) -> Transducer (GenT e2 m) m e1 e2
mapGM f gen = runGenT gen (yield <=< lift . f)

filterG :: Monad m => (e -> Bool) -> Transducer m m e e
filterG p = local f
  where
    f c e | p e = c e
          | otherwise = return ()

foldG :: Monad m => (s -> e -> m s) -> s -> Producer (StateT s m) e -> m s
foldG f s0 p = execStateT (runGenT p fs) s0
  where
    fs e = get >>= lift . flip f e >>= put

-- Mealy-like
streamG :: Monad m => (s -> e1 -> (s, [e2])) -> s -> Transducer (GenT e2 (StateT s m)) m e1 e2
streamG f s0 gen = unStateP (runGenT gen g) s0
  where
    g e = do
        s <- get
        let !(!s', e'm) = f s e
        put s'
        Fold.mapM_ yield e'm

-- Mealy-like
streamGM :: Monad m => (s -> e1 -> m (s, [e2])) -> s -> Transducer (GenT e2 (StateT s m)) m e1 e2
streamGM f s0 gen = unStateP (runGenT gen g) s0
  where
    g e = do
        s <- get
        !(!s', e'm) <- lift . lift $ f s e
        put s'
        Fold.mapM_ yield e'm

unStateP :: Monad m => Producer (StateT s m) e -> s -> Producer m e
unStateP p s0 = unMP (flip evalStateT s0) p

-- | Add an 0-based index to a generated stream.
indexG :: Monad m => Transducer (GenT (Int,e) (StateT Int m)) m e (Int,e)
indexG = streamG (\ix e -> (succ ix,[(ix,e)])) 0

-- | Drop the first 'n' items produced by a 'Generator'.
dropG :: Monad m
     => Int
     -> Transducer (GenT (Int,e) (StateT Int (GenT e m))) m e e
dropG n = mapG snd . filterG ((< n) . fst) . indexG

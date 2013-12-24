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

-----------------------------------------------------------------
-- Transducers

{-# INLINE [1] mapG #-}
mapG :: Monad m => (e1 -> e2) -> Transducer (GenT e2 m) m e1 e2
mapG f gen = runGenT gen (yield . f)
{-# RULES "mapG/mapG" forall f g. mapG f . mapG g = mapG (g . f) . mapG' #-}

mapG' :: Monad m => Transducer (GenT e m) m e e
mapG' gen = runGenT gen yield
{-# NOINLINE [0] mapG'#-}

{-# INLINE [1] mapsG #-}
mapsG :: Monad m => (e1 -> [e2]) -> Transducer (GenT e2 m) m e1 e2
mapsG f gen = runGenT gen (mapM_ yield . f)

mapGM :: Monad m => (e1 -> m e2) -> Transducer (GenT e2 m) m e1 e2
mapGM f gen = runGenT gen (yield <=< lift . f)

{-# RULES "mapsG/mapsG" forall f g. mapsG f . mapsG g = mapsG (f >=> g) . mapG' #-}
{-# RULES "filterG/mapsG" forall f p. filterG p . mapsG f = mapsG (filter p . f) #-}
{-# RULES "mapsG/filterG" forall f p. mapsG f . filterG p = mapsG (\x -> if p x then f x else []) #-}

filterG :: Monad m => (e -> Bool) -> Transducer m m e e
filterG p = local f
  where
    {-# INLINE [0] f #-}
    f c e | p e = c e
          | otherwise = return ()

foldG :: Monad m => (s -> e -> m s) -> s -> Producer (StateT s m) e -> m s
foldG f s0 p = execStateT (runGenT p fs) s0
  where
    {-# INLINE [0] fs #-}
    fs e = get >>= lift . flip f e >>= put

{-# RULES "foldG/mapsG" forall f g s. foldG g s . mapsG f =
    foldG (\acc -> foldM g acc . f) s . mapG' #-}

-- Mealy-like
{-# INLINE [1] streamG #-}
streamG :: Monad m => (s -> e1 -> (s, [e2])) -> s -> Transducer (StateT s (GenT e2 m)) m e1 e2
streamG f s0 gen = const () `liftM` foldG f' s0 gen
  where
    {-# INLINE [0] f' #-}
    f' s e = case f s e of
        (!s', es) -> do
            Fold.mapM_ yield es
            return s'

-- Mealy-like
{-# INLINE [1] streamGM #-}
streamGM :: Monad m => (s -> e1 -> m (s, [e2])) -> s -> Transducer (StateT s (GenT e2 m)) m e1 e2
streamGM f s0 gen = const () `liftM` foldG f' s0 gen
  where
    {-# INLINE [0] f' #-}
    f' s e = do
      !(!s',es) <- lift $ f s e
      Fold.mapM_ yield es
      return s'

-- | Add an 0-based index to a generated stream.
indexG :: Monad m => Transducer (StateT Int (GenT (Int,e) m)) m e (Int,e)
indexG = streamG (\ix e -> (succ ix,[(ix,e)])) 0

-- | Drop the first 'n' items produced by a 'Generator'.
dropG :: Monad m
     => Int
     -> Transducer (StateT Int (GenT (Int,e) (GenT e m))) m e e
dropG n = mapG snd . filterG ((< n) . fst) . indexG

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Stream (
Step (..),
Stream (..),
Streaming (..),

-- * running a 'Stream'
foldS,
foldMStream,
foldMStream_,
runStream_,
transduceStream,

-- * functions
-- ** transformers
maps,
mapsM,
filters,
takes,
drops,
preAnnotate,
postAnnotate,

-- ** aggregating streams
group,
groupVec,
groupVec2,
-- ** utilities
liftFoldS,

-- * low-level stuff (exported for fusion rules)
idStream,
) where

import Prelude hiding (id, (.))
import qualified Prelude as P
import IterX.Core
import IterX.Exception
import IterX.Fusion.Fold

import Control.Exception.Lifted
import Control.Category
import Data.Profunctor
import Data.Traversable
import Control.Monad
import Control.Monad.Base
import Control.Monad.State
import Data.Foldable
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Generic.New as GN
import Control.Monad.ST (runST)
import GHC.IO (unsafeDupablePerformIO)

data Step s o =
    Val s o
  | Skip s
  | End
  deriving (Functor, Foldable, Traversable)

-- a basic stream transforming function.  A 'Stream' produces no more than
-- one output for every input.
data Stream m i o where
    Stream :: (s -> i -> m (Step s o)) -> s -> Stream m i o

instance Monad m => Category (Stream m) where
    {-# INLINE id #-}
    id = idStream
    {-# INLINE (.) #-}
    (.) = cmpStream

{-# INLINE [1] idStream #-}
idStream :: Monad m => Stream m i i
idStream = Stream loop ()
  where
    {-# INLINE [0] loop #-}
    loop _ i = return (Val () i)

{-# INLINE [1] cmpStream #-}
cmpStream :: Monad m => Stream m b c -> Stream m a b -> Stream m a c
cmpStream (Stream rF rs0) (Stream lF ls0) = Stream loop (ls0,rs0)
  where
    {-# INLINE [0] loop #-}
    loop (ls,rs) i0 = do
        lRes <- lF ls i0
        case lRes of
          Val ls' i1 -> do
              rF rs i1 >>= \case
                  Val rs' o -> return (Val (ls',rs') o)
                  Skip rs'  -> return (Skip (ls',rs'))
                  End       -> return End
          Skip ls' -> return (Skip (ls',rs))
          End      -> return End

{-# RULES
"<iterx> id.stream" forall s. cmpStream idStream s = s
"<iterx> stream.id" forall s. cmpStream s idStream = s
    #-}

instance Monad m => Profunctor (Stream m) where
    {-# INLINE dimap #-}
    dimap = dimapStream

{-# INLINE [1] dimapStream #-}
dimapStream :: Monad m => (a->b) -> (c->d) -> Stream m b c -> Stream m a d
dimapStream lf rf (Stream loop s0) = Stream loop' s0
  where
    {-# INLINE [0] loop' #-}
    loop' s b = (liftM . fmap) rf . loop s $ lf b

--------------------------------------------------------

{-# INLINE [1] liftFoldS #-}
liftFoldS :: Monad m => FoldM m a b -> Stream m a b
liftFoldS (FoldM f s0 mkOut) = Stream loop s0
  where
    {-# INLINE [0] loop #-}
    loop s a = do
        s' <- f s a
        Val s' `liftM` mkOut s'

-- -----------------------------------------

{-# INLINE [1] foldS #-}
foldS :: (MonadBase IO m)
      => FoldM m b c
      -> Stream m a b
      -> FoldM m a c
foldS (FoldM f fs0 mkOut) (Stream loopS s0) =
    FoldM loop (fs0, s0) (mkOut . fst)
  where
    {-# INLINE [0] loop #-}
    loop (prev,s) el = loopS s el >>= \case
        Val s' o -> (, s') `liftM` f prev o
        Skip s'  -> return $ (prev, s')
        End      -> throwIO $ TerminateEarly "foldS"

{-# RULES "<iterx> foldS/id" forall f. foldS f idStream = f #-}

{-# INLINE [1] foldMStream #-}
foldMStream :: (MonadBase IO m)
            => (c -> b -> m c)
            -> c
            -> Stream m a b
            -> FoldM m a c
foldMStream f fs0 (Stream loopS s0) = FoldM loop (fs0, s0) out
  where
    {-# INLINE [0] loop #-}
    loop (prev,s) el = loopS s el >>= \case
        Val s' o -> (, s') `liftM` f prev o
        Skip s'  -> return $ (prev, s')
        End      -> throwIO $ TerminateEarly "foldMStream"
    {-# INLINE [0] out #-}
    out = return . fst

{-# INLINE [1] foldMStream_ #-}
foldMStream_ :: (MonadBase IO m) => Stream m a b -> FoldM m a ()
foldMStream_ stream = foldMStream (\_ _ -> return ()) () stream

{-# INLINE [0] runStream_ #-}
runStream_ :: MonadBase IO m
           => Stream m i o
           -> (forall s. Producer (StateT s m) i)
           -> m ()
runStream_ (Stream f s0) gen = const () `liftM` foldG f' s0 gen
  where
    f' s i = f s i >>= \case
        Val s' _ -> return s'
        Skip s'  -> return s'
        End      -> throwIO $ TerminateEarly "runStream_"

{-# INLINE [1] transduceStream #-}
transduceStream :: MonadBase IO m
                => Stream m i o
                -> (forall s. Producer (StateT s (GenT o m)) i)
                -> Producer m o
transduceStream (Stream f s0) gen = foldG f' s0 gen >> return ()
  where
    {-# INLINE [0] f' #-}
    f' s i = lift (f s i) >>= \case
        Val s' o -> yield o >> return s'
        Skip s'  -> return s'
        End      -> throwIO $ TerminateEarly "mkTransducer"

-- -----------------------------------------

{-# RULES
"<iterx> idStream" forall y. cmpStream idStream y = y
    #-}

--------------------------------------------------------
class Streaming p where
    liftStream :: Monad m => Stream m i o -> p m i o

instance Streaming Stream where
    {-# INLINE liftStream #-}
    liftStream = P.id

--------------------------------------------------------

{-# INLINE mapsM #-}
mapsM :: (Streaming p, Monad m) => (i -> m o) -> p m i o
mapsM f = liftStream $ Stream loop ()
  where
    {-# INLINE [0] loop #-}
    loop !() i = liftM (Val ()) (f i)

{-# INLINE maps #-}
maps :: (Streaming p, Monad m) => (i -> o) -> p m i o
maps f = mapsM (return . f)

{-# INLINE filters #-}
filters :: (Streaming p, Monad m) => (i -> Bool) -> p m i i
filters p = liftStream $ Stream loop ()
  where
    {-# INLINE [0] loop #-}
    loop _ i = case p i of
        True  -> return (Val () i)
        False -> return (Skip ())

{-# INLINE takes #-}
takes :: (Streaming p, Monad m) => Int -> p m i i
takes n0 = liftStream $ Stream loop n0
  where
    {-# INLINE [0] loop #-}
    loop n a | n <= 0    = return $ Val (n-1) a
             | otherwise = return End

{-# INLINE drops #-}
drops :: (Streaming p, Monad m) => Int -> p m i i
drops n0 = liftStream $ Stream loop n0
  where
    {-# INLINE [0] loop #-}
    loop n a | n > 0 = return $ Skip (n-1)
             | otherwise = return $ Val 0 a

{-# INLINE group #-}
group :: (Streaming p, Monad m) => Int -> p m i [i]
group n 
    | n == 1 = maps (:[])
    | n > 0 = liftStream $ Stream loop (0,P.id)
    | otherwise = error $ "<iterx> group: n = " ++ show n
  where
    {-# INLINE [0] loop #-}
    loop (cnt,acc) x
      | cnt == (n-1) = return $ Val (0,P.id) (acc [x])
      | otherwise = return $ Skip (cnt+1,acc . (x:))

{-# INLINE groupVec #-}
groupVec :: (Streaming p, MonadBase IO m, G.Vector v i) => Int -> p m i (v i)
groupVec n
    | n > 1 = liftStream $ Stream loop (unsafeDupablePerformIO (GM.unsafeNew n),0)
    | n == 1 = maps (G.singleton)
    | otherwise = error $ "<iterx> groupVec: n " ++ show n
  where
    {-# INLINE [0] loop #-}
    loop (v,thisIx) i
        | thisIx == n-1 = liftBase $ do
            GM.unsafeWrite v thisIx i
            v' <- G.unsafeFreeze v
            v'2 <- GM.unsafeNew n
            return $ Val (v'2,0) v'
        | otherwise = liftBase $ do
            GM.unsafeWrite v thisIx i
            return $ Skip (v,thisIx+1)

-- currently this version seems slower, unless you don't actually
-- use the vector.  Of course it doesn't have the IO dependency...
{-# INLINE groupVec2 #-}
groupVec2 :: forall p m i v. (Streaming p, Monad m, G.Vector v i, GM.MVector (G.Mutable v) i) => Int -> p m i (v i)
groupVec2 n
    | n > 1 = liftStream $ Stream loop (GN.create (GM.unsafeNew n),0)
    | n == 1 = maps (G.singleton)
    | otherwise = error $ "<iterx> groupVec2: n " ++ show n
  where
    {-# INLINE [0] loop #-}
    loop (v,thisIx) i
        | thisIx == n-1 =
            let thisV = runST $ do
                    mv <- GN.run v
                    GM.unsafeWrite mv thisIx i
                    G.unsafeFreeze mv
            in return $
                Val (GN.create (GM.unsafeNew n) :: GN.New v i,0) thisV
        | otherwise = return $ Skip (GN.modify (\mv -> GM.unsafeWrite mv thisIx i) v,thisIx+1)

--------------------------------------------------------

-- would like to make a generic instance for Streaming, but that requires
-- Overlapping.  This isn't so bad anyway.
instance Folding Stream where
    {-# INLINE liftFold #-}
    liftFold = liftFoldS

--------------------------------------------------------

{-# INLINE preAnnotate #-}
-- annotate a Stream output by a 'FoldM'.  The fold is updated before
-- the stream, so the fold state will be updated even if the stream doesn't
-- produce an output value at a step
preAnnotate :: (Streaming p, Monad m)
            => Stream m a b -> FoldM m a c -> p m a (b,c)
preAnnotate (Stream sf ss0) (FoldM ff fs0 mkOut) =
    liftStream $ Stream loop (ss0,fs0)
  where
    {-# INLINE [0] loop #-}
    loop (ss,fs) a = do
        fs' <- ff fs a
        sf ss a >>= \case
            Val ss' b -> (Val (ss',fs') . (b,)) `liftM` mkOut fs'
            Skip ss'  -> return $ Skip (ss',fs')
            End       -> return End

{-# INLINE postAnnotate #-}
-- annotate a Stream output with a 'FoldM' value.  The fold is updated after
-- the stream, and only when the stream produces an output value
postAnnotate :: (Streaming p, Monad m)
             => Stream m a b -> FoldM m a c -> p m a (b,c)
postAnnotate (Stream sf ss0) (FoldM ff fs0 mkOut) =
    liftStream $ Stream loop (ss0,fs0)
  where
    {-# INLINE [0] loop #-}
    loop (ss,fs) a = sf ss a >>= \case
            Val ss' b -> do
                fs' <- ff fs a
                c <- mkOut fs'
                return $ Val (ss',fs') (b,c)
            Skip ss'  -> return $ Skip (ss',fs)
            End       -> return End


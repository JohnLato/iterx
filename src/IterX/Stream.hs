{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -Wall #-}
module IterX.Stream where

import Prelude hiding (id, (.))
import qualified Prelude as P
import IterX.Core
import IterX.Exception

import Control.Exception.Lifted
import Control.Category
import Data.Profunctor
import Data.Traversable
import Control.Monad
import Control.Monad.Base
import Control.Monad.State
import Data.Foldable
-- import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Generic as G

data Step s o =
    Val s o
  | Skip s
  | End
  deriving (Functor, Foldable, Traversable)

-- ultimate goal: be able to freely combine Streams with Iteratees.
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
    loop !() i = return (Val () i)

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

-- A Fold is like a Stream, but there are no intermediate output values.
-- There's also no indication of termination.
--
-- FoldM is strictly stronger than stream, because it also has an initial
-- value.
data FoldM m a b where
  FoldM :: (s -> a -> m s) -> s -> (s -> m b) -> FoldM m a b

{-# INLINE [1] liftFoldS #-}
liftFoldS :: Monad m => FoldM m a b -> Stream m a b
liftFoldS (FoldM f s0 mkOut) = Stream loop s0
  where
    {-# INLINE [0] loop #-}
    loop s a = do
        s' <- f s a
        Val s' `liftM` mkOut s'

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

{-# INLINE [1] foldS #-}
foldS :: (MonadBase IO m)
      => FoldM m b c
      -> Stream m a b
      -> FoldM m a c
foldS (FoldM f fs0 mkOut) (Stream loopS s0) = FoldM loop (fs0, s0) out
  where
    {-# INLINE [0] loop #-}
    loop (prev,s) el = loopS s el >>= \case
        Val s' o -> (, s') `liftM` f prev o
        Skip s'  -> return $ (prev, s')
        End      -> throwIO $ TerminateEarly "foldMStream"
    {-# INLINE [0] out #-}
    out = mkOut . fst

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

{-# INLINE [0] runFold #-}
runFold :: Monad m => FoldM m i o -> (forall s. Producer (StateT s m) i) -> m o
runFold (FoldM f s0 mkOut) gen = foldG f s0 gen >>= mkOut

{-# INLINE [0] runFold_ #-}
runFold_ :: Monad m => FoldM m i o ->  (forall s. Producer (StateT s m) i) -> m ()
runFold_ (FoldM f s0 _mkOut) gen = const () `liftM` foldG f s0 gen

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

{-# INLINE [1] transduceY #-}
transduceY :: MonadBase IO m
           => Y m i o
           -> (forall s. Producer (StateT s (GenT o m)) i)
           -> Producer m o
transduceY (Y (UnfoldM mkUnf uf) (Stream sf1 s1_0) (Stream sf2 s2_0)) gen =
    foldG f' (s1_0, s2_0) gen >> return ()
  where
    {-# INLINE [0] f' #-}
    f' (s1,s2) i = lift (sf1 s1 i) >>= \case
        Val s1' x -> inner s1' s2 (mkUnf x)
        Skip s1'  -> return (s1',s2)
        End       -> throwIO $ TerminateEarly "<iterX> transduceY: outer loop"
    {-# INLINE [0] inner #-}
    inner s1 s20 unfS0 = go s20 unfS0
      where
        go s2 unfS = lift (uf unfS) >>= \case
            Just (a,unfS') -> lift (sf2 s2 a) >>= \case
                Val s2' o -> yield o >> go s2' unfS'
                Skip s2'  -> go s2' unfS'
                End       -> throwIO $ TerminateEarly "<iterX> transduceY: inner loop"
            Nothing -> return (s1,s2)

-- -----------------------------------------

{-
{-# INLINE [1] fromStream  #-}
fromStream :: MonadIO m => Int -> Stream m Any o -> m (V.Vector o)
fromStream sz str = do
    v <- liftIO $ VM.unsafeNew sz
    let putE i !x = liftIO (VM.unsafeWrite v i x) >> return (i+1)
    case foldMT putE 0 . str of
        (Stream f s0) -> do
            let go o s = do
                    f s aNY >>= \case
                      Val s' o' -> go o' s'
                      Skip s'   -> go o  s'
                      End       -> return o
            n' <- go 0 s0
            liftIO $ V.unsafeFreeze $ VM.unsafeSlice 0 n' v
            -}

-- -----------------------------------------
-- Unfoldings
data UnfoldM m full a = forall s. UnfoldM (full -> s) (s -> m (Maybe (a,s)))

{-# INLINE [1] unfoldIdM #-}
unfoldIdM :: Monad m => UnfoldM m a a
unfoldIdM = UnfoldM Just $ \x -> case x of
    Just a  -> return $ Just (a,Nothing)
    Nothing -> return Nothing

newtype Stepper b = Stepper { unStepper :: Maybe (b, Stepper b) }

{-# INLINE [1] unfoldVec #-}
unfoldVec :: (G.Vector v a, Monad m) => UnfoldM m (v a) a
unfoldVec = UnfoldM mkS loop
  where
    {-# INLINE [0] mkS #-}
    mkS v =
        let f i | i < G.length v =
                    Stepper $ Just (G.unsafeIndex v i, f (i+1))
                | otherwise = (Stepper Nothing)
        in f 0
    {-# INLINE [0] loop #-}
    loop (Stepper this) = return this

-- this currently performs much better than the closure-based unfolding
{-# INLINE [1] unfoldVec2 #-}
unfoldVec2 :: (G.Vector v a, Monad m) => UnfoldM m (v a) a
unfoldVec2 = UnfoldM mkS f
  where
    {-# INLINE [0] mkS #-}
    mkS v = (0,G.length v, v)
    {-# INLINE [0] f #-}
    f (i,n,v) | i < n = return $ Just (G.unsafeIndex v i,(i+1,n,v))
              | otherwise = return Nothing

{-# INLINE [1] unfoldList #-}
unfoldList :: Monad m => UnfoldM m [a] a
unfoldList = UnfoldM P.id f
  where
    f (x:xs) = return $ Just (x,xs)
    f []     = return Nothing

-- create a new unfolding from two other unfoldings and a 'Stream'.
-- really only useful for composing 'Y' values.
{-# INLINE [1] newUnf #-}
newUnf :: Monad m
       => Stream m el1 full2
       -> UnfoldM m full1 el1
       -> UnfoldM m full2 el2
       -> UnfoldM m full1 el2
newUnf (Stream fStrm ss0) (UnfoldM uf10 uf1) (UnfoldM uf20 uf2) = UnfoldM (\full -> (ss0,uf10 full,Nothing)) go
  where
    {-# INLINE [0] go #-}
    go (ss,s,Just akt) = go2 ss s akt
    go (ss,s,Nothing) = uf1 s >>= \case
      Just (el1,s1') -> fStrm ss el1 >>= \case
          Val ss' full2 ->
              go2 ss' s1' $ uf2 $ uf20 full2
          Skip ss' -> go (ss',s1',Nothing)
          End -> return Nothing
      Nothing -> return Nothing
    {-# INLINE [0] go2 #-}
    go2 ss s akt = akt >>= \case
        Just (el,f2s') ->
            return $ Just (el,(ss,s,Just $ uf2 f2s'))
        Nothing -> go (ss,s,Nothing)

{-# INLINE [1] foldUnfolding #-}
foldUnfolding :: Monad m => UnfoldM m a b -> FoldM m b c -> FoldM m a c
foldUnfolding (UnfoldM mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\s a -> loop2 (mkUnf a) s) s0 mkOut
  where
    -- it's much faster to leave this un-INLINEd
    loop2 unfState foldState = uf unfState >>= \case
        Just (a, unfState') -> f foldState a >>= loop2 unfState'
        Nothing -> return foldState

{-# RULES "<iterx> fold/unfoldId" forall f. foldUnfolding unfoldIdM f = f #-}

-- -----------------------------------------

{-
type TransduceFold m a b = forall c. FoldM m b c -> FoldM m a c

{-# INLINE [1] byStream #-}
byStream :: Monad m => Stream m a b -> TransduceFold m a b
byStream (Stream fS s0S) (FoldM fEl s0El elOut) =
    FoldM loop (Just s0S, s0El) (elOut . snd)
  where
    {-# INLINE [0] loop #-}
    loop (Just sState, foldState) full = fS sState full >>= \case
        Val sState' o -> fEl foldState o >>= \fs' -> return (Just sState',fs')
        Skip sState'  -> return (Just sState',foldState)
        End           -> return (Nothing,foldState)
    loop s@(Nothing, _) _ = return s

{-# RULES "<iterx> byStream/byStream" forall s t. byStream s . byStream t = byStream (t . s) #-}
{-# RULES <iterx> "byStream (byStream)" forall s t x. byStream s (byStream t x) = byStream (t . s) x #-}

-- not entirely pleased with this yet, maybe this Y type is better?
-}

-- this is basically a hylomorphism, and seems to have the properties I want.
-- 'Y' is even more general than a basic Stream.
data Y m i o where
    Y :: UnfoldM m full elem -> Stream m i full -> Stream m elem o -> Y m i o

instance Monad m => Category (Y m) where
    {-# INLINE id #-}
    id = idY
    {-# INLINE (.) #-}
    (.) = cmpY

{-# INLINE [1] idY #-}
idY :: Monad m => Y m a a
idY = Y unfoldIdM s f
  where
    s = Stream (\() a -> return (Val () a)) ()
    f = Stream (\() a -> return (Val () a)) ()

-- composition is nice, we only need to create a new unfolding from
-- an intermediate stream.
{-# INLINE [1] cmpY #-}
cmpY :: Monad m => Y m b c -> Y m a b -> Y m a c
cmpY (Y uf2 s3 s4) (Y uf1 s1 s2) = Y uf s1 s4
  where
    {-# INLINE uf #-}
    uf = newUnf (s3 . s2) uf1 uf2

instance Monad m => Profunctor (Y m) where
    {-# INLINE dimap #-}
    dimap = dimapY

dimapY :: Monad m => (a->b) -> (c->d) -> Y m b c -> Y m a d
dimapY lf rf (Y unf s1 s2) = Y unf (lmap lf s1) (rmap rf s2)

{-# INLINE [1] liftS #-}
liftS :: Monad m => Stream m a b -> Y m a b
liftS s = Y unfoldIdM s id
  -- id could go on either side, but if it's on the right, then compositions
  -- have an inner 'id . stream', which can be more easily removed by RULES.

-- possible rewrites:
--  newUnf: create a simplified version for id stream (probably not necessary)

{-# RULES "<iterx> liftS (liftS)" forall l r. cmpY (liftS l) (liftS r) = liftS (l . r) #-}
-- we need this one because composition is right-associative by default
{-# RULES "<iterx> liftS/cmpY liftS" forall l r z. cmpY (liftS l) (cmpY (liftS r) z) = cmpY (liftS (l . r)) z #-}
{-# RULES "<iterx> liftS/liftS" forall l r. liftS l . liftS r = liftS (l . r) #-}

{-# RULES "<iterx> liftS/id" liftS idStream = idY #-}
{-# RULES "<iterx> idY" forall y. cmpY idY y = y #-}
{-# RULES "<iterx> idStream" forall y. cmpStream idStream y = y #-}

-- composing 'Stream' with 'Y' can be efficient
cmpStreamY :: Monad m => Stream m b c -> Y m a b -> Y m a c
cmpStreamY s (Y unf s1 s2) = Y unf s1 (s . s2)

cmpYStream :: Monad m => Y m b c -> Stream m a b -> Y m a c
cmpYStream (Y unf s1 s2) s = Y unf (s1 . s) s2

{-# RULES "<iterx> cmpY/stream" forall s y. cmpY (liftS s) y = cmpStreamY s y #-}
{-# RULES "<iterx> stream/cmpY" forall s y. cmpY y (liftS s) = cmpYStream y s #-}

{-# INLINE [1] foldY #-}
-- fold over a 'Y' structure
foldY :: MonadBase IO m => FoldM m b c -> Y m a b -> FoldM m a c
foldY finalFold (Y unf s1 s2) =
    foldS (foldUnfolding unf $ foldS finalFold s2) s1

{-# RULES "<iterx> foldY lowering" forall f s. foldY f (liftS s) = foldS f s #-}

{-# INLINE unfolding #-}
unfolding :: Monad m => UnfoldM m a b -> Y m a b
unfolding unf = Y unf id id

--------------------------------------------------------
class Streaming p where
    liftStream :: Monad m => Stream m i o -> p m i o

instance Streaming Stream where
    {-# INLINE liftStream #-}
    liftStream = P.id

instance Streaming Y where
    {-# INLINE liftStream #-}
    liftStream = liftS

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
    loop !() i = case p i of
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

--------------------------------------------------------

class Folding p where
    liftFold :: Monad m => FoldM m i o -> p m i o

instance Folding FoldM where
    liftFold = P.id

-- would like to make a generic instance for Streaming, but that requires
-- Overlapping.  This isn't so bad anyway.
instance Folding Stream where
    {-# INLINE liftFold #-}
    liftFold = liftFoldS

instance Folding Y where
    {-# INLINE liftFold #-}
    liftFold = liftS . liftFoldS

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

{-# INLINE [1] zippingWith #-}
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

{-# INLINE [1] preAnnotate #-}
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

{-# INLINE [1] postAnnotate #-}
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


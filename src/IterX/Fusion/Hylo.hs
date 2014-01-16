{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Hylo (
Y(..),

-- * running a 'Y'
foldY,
transduceY,

-- * creating 'Y'
unfolding,
refolding,

-- * temporary
initStream,
initStream2,
) where

import Prelude hiding (id, (.))
import qualified Prelude as P
import IterX.Core
import IterX.Exception
import IterX.Fusion.Fold
import IterX.Fusion.Stream
import IterX.Fusion.Unfold
import IterX.IterX

import Control.Exception.Lifted
import Control.Category
import Data.Profunctor
import Control.Monad.Base
import Control.Monad.State


-- | An abstract hylomorphism transformation.
--
-- Given an input stream 'i', 'Y' creates a new stream of an
-- abstract type, performs a hylomorphism (unfold/fold),
-- and generates an output stream of type 'o'.
--
-- 'Y' is more general than a basic Stream.
data Y m i o where
    Y :: UnfoldM m full elem -> Stream m i full -> Stream m elem o -> Y m i o

instance Monad m => Category (Y m) where
    {-# INLINE id #-}
    id = idY
    {-# INLINE (.) #-}
    (.) = cmpY

{-# INLINE [1] idY #-}
idY :: Monad m => Y m a a
idY = Y unfoldIdM idStream idStream

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

instance Streaming Y where
    {-# INLINE liftStream #-}
    liftStream = liftS

instance Folding Y where
    {-# INLINE liftFold #-}
    liftFold = liftS . liftFoldS

-- -----------------------------------------

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

transduceY (Y (SUnfoldM unf0 mkUnf uf) (Stream sf1 s1_0) (Stream sf2 s2_0)) gen =
    foldG f' (unf0, s1_0, s2_0) gen >> return ()
  where
    {-# INLINE [0] f' #-}
    f' (sUnf, s1,s2) i = lift (sf1 s1 i) >>= \case
        Val s1' x -> inner s1' s2 (mkUnf sUnf x)
        Skip s1'  -> return (sUnf, s1',s2)
        End       -> throwIO $ TerminateEarly "<iterX> transduceY: outer loop"
    {-# INLINE [0] inner #-}
    inner s1 s20 unfS0 = go s20 unfS0
      where
        go s2 unfS = lift (uf unfS) >>= \case
            Right (a,unfS') -> lift (sf2 s2 a) >>= \case
                Val s2' o -> yield o >> go s2' unfS'
                Skip s2'  -> go s2' unfS'
                End       -> throwIO $ TerminateEarly "<iterX> transduceY: inner loop"
            Left sUnf' -> return (sUnf', s1, s2)


-- -----------------------------------------

-- create a new unfolding from two other unfoldings and a 'Stream'.
-- really only useful for composing 'Y' values, which is why it lives
-- in this module.
--
-- this function has gotten really gross with two types of unfoldings.
{-# INLINE [1] newUnf #-}
newUnf :: Monad m
       => Stream m el1 full2
       -> UnfoldM m full1 el1
       -> UnfoldM m full2 el2
       -> UnfoldM m full1 el2
newUnf (Stream fStrm ss0) (UnfoldM uf10 uf1) (UnfoldM uf20 uf2) =
    SUnfoldM ss0 (\ss full -> (ss,uf10 full,Nothing)) go
    -- in the case where ss0 :: (), we can leave this as a simple
    -- unfolding.  But that's more work for now, and we may stick with
    -- complex unfoldings the whole time in the end, so I can add it later
  where
    {-# INLINE [0] go #-}
    go (ss,s,Just akt) = go2 ss s akt
    go (ss,s,Nothing) = uf1 s >>= \case
      Just (el1,s1') -> fStrm ss el1 >>= \case
          Val ss' full2 ->
              go2 ss' s1' $ uf2 $ uf20 full2
          Skip ss' -> go (ss',s1',Nothing)
          End -> throw $ TerminateEarly "<iterx> newUnf"
      Nothing -> return $ Left ss
    {-# INLINE [0] go2 #-}
    go2 ss s akt = akt >>= \case
        Just (el,f2s') ->
            return $ Right (el,(ss,s,Just $ uf2 f2s'))
        Nothing -> go (ss,s,Nothing)

newUnf (Stream fStrm ss0) (SUnfoldM ufs10 uf10 uf1) (UnfoldM uf20 uf2) =
        SUnfoldM (ufs10,ss0)
                 (\(ufs1,ss) full -> (ss,uf10 ufs1 full,Nothing))
                 go
  where
    {-# INLINE [0] go #-}
    go (ss,s,Just akt) = go2 ss s akt
    go (ss,s,Nothing) = uf1 s >>= \case
      Right (el1,s1') -> fStrm ss el1 >>= \case
          Val ss' full2 ->
              go2 ss' s1' $ uf2 $ uf20 full2
          Skip ss' -> go (ss',s1',Nothing)
          End -> throw $ TerminateEarly "<iterx> newUnf"
      Left ufs1' -> return $ Left (ufs1',ss)
    {-# INLINE [0] go2 #-}
    go2 ss s akt = akt >>= \case
        Just (el,f2s') ->
            return $ Right (el,(ss,s,Just $ uf2 f2s'))
        Nothing -> go (ss,s,Nothing)

newUnf (Stream fStrm ss0) (UnfoldM uf10 uf1) (SUnfoldM ufs20 uf20 uf2) =
        SUnfoldM (ufs20,ss0)
                 (\(ufs2,ss) full -> (ss,uf10 full,Left ufs2))
                 go
  where
    {-# INLINE [0] go #-}
    go (ss,full,Right akt) = go2 ss full akt
    go (ss,full,Left ufs2) = uf1 full >>= \case
        Just (el1,s1') -> fStrm ss el1 >>= \case
          Val ss' full2 -> go2 ss' s1' $ uf2 $ uf20 ufs2 full2
          Skip ss'      -> go (ss',s1',Left ufs2)
          End -> throw $ TerminateEarly "<iterx> newUnf"
        Nothing -> return $ Left (ufs2,ss)
    {-# INLINE [0] go2 #-}
    go2 ss full akt = akt >>= \case
        Right (el,f2s') ->
            return $ Right (el,(ss,full,Right $ uf2 f2s'))
        Left ufs2' -> go (ss,full,Left ufs2')

newUnf (Stream fStrm ss0) (SUnfoldM ufs10 uf10 uf1) (SUnfoldM ufs20 uf20 uf2) =
        SUnfoldM (ufs10,ufs20,ss0)
                 (\(ufs1,ufs2,ss) full -> (ss,uf10 ufs1 full,Left ufs2))
                 go
  where
    {-# INLINE [0] go #-}
    go (ss,full,Right akt) = go2 ss full akt
    go (ss,full,Left ufs2) = uf1 full >>= \case
        Right (el1,s1') -> fStrm ss el1 >>= \case
          Val ss' full2 -> go2 ss' s1' $ uf2 $ uf20 ufs2 full2
          Skip ss'      -> go (ss',s1',Left ufs2)
          End -> throw $ TerminateEarly "<iterx> newUnf"
        Left ufs1' -> return $ Left (ufs1',ufs2,ss)
    {-# INLINE [0] go2 #-}
    go2 ss full akt = akt >>= \case
        Right (el,f2s') ->
            return $ Right (el,(ss,full,Right $ uf2 f2s'))
        Left ufs2' -> go (ss,full,Left ufs2')

-- Fold over an unfolding.
{-# INLINE [1] foldUnfolding #-}
foldUnfolding :: Monad m => UnfoldM m a b -> FoldM m b c -> FoldM m a c
foldUnfolding (UnfoldM mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\s a -> loop2 (mkUnf a) s) s0 mkOut
  where
    -- INLINE-ing this is a big loss.
    loop2 unfState foldState = uf unfState >>= \case
        Just (a, unfState') -> f foldState a >>= loop2 unfState'
        Nothing -> return foldState
foldUnfolding (SUnfoldM unfS0 mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\(unfS,s) a -> loop2 (mkUnf unfS a) s) (unfS0,s0) (mkOut.snd)
  where
    loop2 unfState foldState = uf unfState >>= \case
        Right (a, unfState') -> f foldState a >>= loop2 unfState'
        Left unfState' -> return (unfState',foldState)

-- For each input, unfold it, fold over that, and produce one output.
-- Essently, undo an UnfoldM.
-- No fold state is carried over across separate inputs.
{-# INLINE [1] repeatedFoldUnfolding #-}
repeatedFoldUnfolding :: Monad m => UnfoldM m a b -> FoldM m b c -> Stream m a c
repeatedFoldUnfolding (UnfoldM mkUnf uf) (FoldM f s0 mkOut) =
    Stream (\_ a -> loop (mkUnf a) s0) ()
  where
    -- INLINE-ing this is a big loss.
    loop unfState foldState = uf unfState >>= \case
        Just (a, unfState') -> f foldState a >>= loop unfState'
        Nothing -> Val () `liftM` mkOut foldState

repeatedFoldUnfolding (SUnfoldM unfS0 mkUnf uf) (FoldM f s0 mkOut) =
    Stream (\unfS a -> loop (mkUnf unfS a) s0) unfS0
  where
    loop unfState foldState = uf unfState >>= \case
        Right (a, unfState') -> f foldState a >>= loop unfState'
        Left unfState' -> Val unfState' `liftM` mkOut foldState

{-# RULES "<iterx> fold/unfoldId" forall f. foldUnfolding unfoldIdM f = f #-}

--------------------------------------------------------

{-# INLINE [1] liftS #-}
liftS :: Monad m => Stream m a b -> Y m a b
liftS s = Y unfoldIdM s id
-- id could go on either side, but if it's on the right, then composition
-- has an inner 'id . stream', which can be more easily removed by RULES

-- we need this one because composition is right-associative by default
{-# RULES
"<iterx> liftS (liftS)" forall l r. cmpY (liftS l) (liftS r)
          = liftS (l . r)
"<iterx> liftS/cmpY liftS"
         forall l r z. cmpY (liftS l) (cmpY (liftS r) z)
           = cmpY (liftS (l . r)) z
"<iterx> liftS/liftS" forall l r. liftS l . liftS r = liftS (l . r)

"<iterx> liftS/id" liftS idStream = idY
"<iterx> idY" forall y. cmpY idY y = y
    #-}

-- composing 'Stream' with 'Y' can be efficient
cmpStreamY :: Monad m => Stream m b c -> Y m a b -> Y m a c
cmpStreamY s (Y unf s1 s2) = Y unf s1 (s . s2)

cmpYStream :: Monad m => Y m b c -> Stream m a b -> Y m a c
cmpYStream (Y unf s1 s2) s = Y unf (s1 . s) s2

{-# RULES
"<iterx> cmpY/stream" forall s y. cmpY (liftS s) y = cmpStreamY s y
"<iterx> cmpYStream/id" forall y. cmpStreamY idStream y = y

"<iterx> stream/cmpY" forall s y. cmpY y (liftS s) = cmpYStream y s
"<iterx> id/cmpYStream" forall y. cmpYStream y idStream = y
    #-}

{-# INLINE foldY #-}
-- fold over a 'Y' structure
foldY :: MonadBase IO m => FoldM m b c -> Y m a b -> FoldM m a c
foldY finalFold (Y unf s1 s2) =
    foldS (foldUnfolding unf $ foldS finalFold s2) s1

{-# RULES "<iterx> foldY lowering" forall f s. foldY f (liftS s) = foldS f s #-}

{-# INLINE unfolding #-}
unfolding :: Monad m => UnfoldM m a b -> Y m a b
unfolding unf = Y unf idStream idStream

{-# INLINE refolding #-}
refolding :: (Streaming p, MonadBase IO m)
          => FoldM m b c -> Y m a b -> p m a c
refolding fold (Y unf s1 s2) =
    liftStream $ (repeatedFoldUnfolding unf $ foldS fold s2) . s1

{-# INLINE initStream #-}
initStream :: (Streaming p, Monad m)
           => IterX i m st -> (st -> Stream m i o) -> p m i o
initStream iter f = liftStream $ f2 f . f1 iter

-- the unpacking/repacking we have to do here (esp. in f2) feels
-- like the wrong thing.  Maybe there's a better way?
{-# INLINE f1 #-}
f1 :: Monad m => IterX i m st -> Stream m i (st,i)
f1 iter = Stream loop StartDelimiter
  where
    {-# INLINE [0] loop #-}
    loop StartDelimiter i = runIter iter i HasMore failX doneX >>= procRes
    loop (ConsumeDelimiter k) i = k i >>= procRes
    loop p@(ProcState st) i = return $ Val p (st,i)
    {-# INLINE [0] procRes #-}
    procRes res = case res of
      MoreX k' -> return $ Skip $ ConsumeDelimiter k'
      DoneX s' rest -> return $ Val (ProcState s') (s',rest)
      FailX _ err -> throw $ IterFailure $ "<iterx> f1: " ++ err

-- assume that, once we get an 'st', it's constant.
{-# INLINE f2 #-}
f2 :: Monad m => (st -> Stream m i o) -> Stream m (st,i) o
f2 f = Stream loop Nothing
  where
    {-# INLINE [0] loop #-}
    loop Nothing (s,i) = case f s of
        Stream f' s' -> f' s' i >>= \case
          Val s'2 o -> return $ Val (Just $ Stream f' s'2) o
          Skip s'2 -> return $ Skip (Just $ Stream f' s'2)
          End -> return End
    loop (Just (Stream f' s')) (_,i) = f' s' i >>= \case
          Val s'2 o -> return $ Val (Just $ Stream f' s'2) o
          Skip s'2 -> return $ Skip (Just $ Stream f' s'2)
          End -> return End

newtype Stepper m i o = Stepper (i -> m (Maybe (Maybe o, Stepper m i o)))

-- implement the same semantics as initStream by closing over the next
-- loop.  I'd hoped this would be more efficient, but it does not seem to
-- be, at least for my minimal test cases thus far.
{-# INLINE initStream2 #-}
initStream2 :: (Streaming p, Monad m)
            => IterX i m st -> (st -> Stream m i o) -> p m i o
initStream2 iter fStream0 = liftStream $ Stream loop s0
  where
    {-# INLINE [0] loop #-}
    loop (Stepper f) i = f i >>= \case
        Just (Nothing,next) -> return $ Skip next
        Just (Just o,next)  -> return $ Val next o
        Nothing             -> return End
    {-# INLINE s0 #-}
    s0 = Stepper $ \i -> runIter iter i HasMore failX doneX >>= procResult
    {-# INLINE procResult #-}
    procResult res = case res of
        MoreX k' -> return $ Just (Nothing, Stepper $ k' >=> procResult)
        DoneX s' r -> g' (fStream0 s') r
        FailX _ err -> throw $ IterFailure $ "initStream2: " ++ err
    g' (Stream f s) i = f s i >>= \case
        Val s' o -> return $ Just (Just o, Stepper $ g' (Stream f s'))
        Skip s'  -> return $ Just (Nothing, Stepper $ g' (Stream f s'))
        End      -> return Nothing

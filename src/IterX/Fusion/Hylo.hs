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
) where

import Prelude hiding (id, (.))
import qualified Prelude as P
import IterX.Core
import IterX.Exception
import IterX.Fusion.Fold
import IterX.Fusion.Stream
import IterX.Fusion.Unfold

import Control.Exception.Lifted
import Control.Category
import Data.Profunctor
import Control.Monad.Base
import Control.Monad.State


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
    -- it's much faster to leave this un-INLINEd for simple tests,
    -- but on prodTest4, it makes the regular vector unfolding
    -- more efficient.  Need more data to know what's best.
    loop2 unfState foldState = uf unfState >>= \case
        Just (a, unfState') -> f foldState a >>= loop2 unfState'
        Nothing -> return foldState
foldUnfolding (SUnfoldM unfS0 mkUnf uf) (FoldM f s0 mkOut) =
    FoldM (\(unfS,s) a -> loop2 (mkUnf unfS a) s) (unfS0,s0) (mkOut.snd)
  where
    -- it's much faster to leave this un-INLINEd for simple tests,
    -- but on prodTest4, it makes the regular vector unfolding
    -- more efficient.  Need more data to know what's best.
    loop2 unfState foldState = uf unfState >>= \case
        Right (a, unfState') -> f foldState a >>= loop2 unfState'
        Left unfState' -> return (unfState',foldState)

{-# RULES "<iterx> fold/unfoldId" forall f. foldUnfolding unfoldIdM f = f #-}

--------------------------------------------------------


-- -----------------------------------------

{-# INLINE [1] liftS #-}
liftS :: Monad m => Stream m a b -> Y m a b
liftS s = Y unfoldIdM s id
  -- id could go on either side, but if it's on the right, then compositions
  -- have an inner 'id . stream', which can be more easily removed by RULES.


-- we need this one because composition is right-associative by default
{-# RULES
"<iterx> liftS (liftS)" forall l r. cmpY (liftS l) (liftS r)
          = liftS (l . r)
"<iterx> liftS/cmpY liftS"
         forall l r z. cmpY (liftS l) (cmpY (liftS r) z)
           = cmpY (liftS (l . r)) z
"<iterx> liftS/liftS" forall l r. liftS l . liftS r = liftS (l . r)
    #-}

{-# RULES
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
    #-}

{-# RULES
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

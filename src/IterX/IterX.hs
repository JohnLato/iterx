{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TupleSections             #-}

{-# OPTIONS -Wall #-}
module IterX.IterX (
  ResultX(..)
, IterX(..)
, Status(..)
, Failure
, Success

, failX
, doneX
, cxtFailure

, feed

, runIterX
, convStream
, unfoldConvStream

, iterXToStreamTrans
, delimitG
, delimitN
, DelState
) where

import           IterX.Core
import           IterX.Exception
import           IterX.StreamTrans
import           IterX.Unsafe

import           Control.Applicative
import           Control.Monad.State
import           Control.Exception (throw)
import           Data.MonoTraversable
import           Data.Monoid
import           Data.Sequences

----------------------------------------------------------------

data ResultX s m r =
    DoneX r s
  | FailX s String
  | MoreX (s -> m (ResultX s m r))
    deriving (Functor)

instance (Show s, Show r) => Show (ResultX s m r) where
    show (DoneX r s) = "DoneX (" ++ show r ++ ") (" ++ show s ++ ")"
    show (FailX s e) = "FailX (" ++ show s ++ ") (" ++ show e ++ ")"
    show (MoreX _)   = "MoreX"

data Status = EOF | HasMore
    deriving (Eq, Show)

newtype IterX s m a = IterX {
    runIter :: forall r.
               s
               -> Status
               -> Failure s m r
               -> Success s m a r
               -> m (ResultX s m r)}

type Failure s m r   = s -> Status -> String -> m (ResultX s m r)
type Success s m a r = s -> Status -> a -> m (ResultX s m r)

instance Functor (IterX s m) where
    {-# INLINE fmap #-}
    fmap   = mapIter

instance Applicative (IterX s m) where
    {-# INLINE pure #-}
    pure   = returnIter
    {-# INLINE (<*>) #-}
    (<*>)  = apIter

instance Monad (IterX s m) where
    {-# INLINE return #-}
    return = returnIter
    {-# INLINE (>>=) #-}
    (>>=)  = bindIter
    fail   = failIter

instance MonadTrans (IterX s) where
    {-# INLINE lift #-}
    lift m = IterX $ \s st _ onD -> m >>= onD s st

failX :: Monad m => s -> Status -> String -> m (ResultX s m a)
failX s _ err = return $ FailX s err

doneX :: Monad m => s -> Status -> a -> m (ResultX s m a)
doneX s _ a   = return $ DoneX a s

-- | transform a failure continuation by adding extra context
cxtFailure :: String -> IterX s m a -> IterX s m a
cxtFailure cxt i = IterX $ \s st onF onD ->
      let cxtF s' st' err = onF s' st' (cxt ++ ": " ++ err)
      in  runIter i s st cxtF onD
{-# INLINE cxtFailure #-}

returnIter :: a -> IterX s m a
returnIter a = IterX $ \s st _ onD -> onD s st a
{-# INLINE returnIter #-}

bindIter :: IterX s m a -> (a -> IterX s m b) -> IterX s m b
bindIter m f = IterX $ \s st onF onD -> runIter m s st onF
    $ \s' st' a -> runIter (f a) s' st' onF onD
{-# INLINE bindIter #-}

failIter :: String -> IterX s m a
failIter err = IterX $ \s st onF _ -> onF s st err
{-# INLINE failIter #-}

mapIter :: (a -> b) -> IterX s m a -> IterX s m b
mapIter f m = IterX $ \s st onF onD -> runIter m s st onF $
     \s' st' a -> onD s' st' (f a)
{-# INLINE mapIter #-}

apIter :: IterX s m (a -> b) -> IterX s m a -> IterX s m b
apIter f a = do
    f' <- f
    a' <- a
    return (f' a')
{-# INLINE apIter #-}

----------------------------------------------------------------

-- run an iteratee with the given producer.  Extra input after the iteratee
-- completes is discarded.
runIterX :: (Monad m)
         => Producer (StateT (ResultX s m a) m) s
         -> IterX s m a
         -> m (ResultX s m a)
runIterX gen i = do
    let s0 = MoreX $ \s -> runIter i s HasMore failX doneX
    foldG f s0 gen
  where
    f (MoreX k) s   = k s
    f (DoneX _ _) _ = throw (TerminateEarly "runIterX")
    f r _ = return r

----------------------------------------------------------------

feed :: (Monad m, Monoid s) => ResultX s m r -> s -> m (ResultX s m r)
feed (MoreX k)    s = k s
feed (DoneX r s0) s = return $ DoneX r (s0<>s)
feed f@FailX{}    _ = return f

----------------------------------------------------------------

-- | Create a 'Transducer' from an 'IterX'
convStream :: Monad m
           => IterX e1 m e2
           -> Transducer (StateT (ResultX e1 m e2) (GenT e2 m)) m e1 e2
convStream i = streamGM (f id) i0
  where
    i0 = MoreX $ \inp -> runIter i inp HasMore failX doneX
    f acc (MoreX k) s =  k s >>= \case
        res@(MoreX _) -> return (res,acc [])
        -- TODO: probably makes sense to unroll this at least once
        DoneX e2 rest -> f (acc . (e2:)) i0 rest
        FailX _ err -> throw $ IterFailure $ "convStream: " ++ err
    f _acc _other _ = error "convStream: other case arrived?"

-- | Create a 'Transducer' from an 'IterX' generating function
unfoldConvStream :: Monad m
                 => (st -> IterX e1 m (st,e2))
                 -> st
                 -> Transducer (StateT (ResultX e1 m (st,e2)) (GenT e2 m)) m
                      e1 e2
unfoldConvStream mkI st0 = streamGM (f id) (i0 st0)
  where
    i0 s = MoreX $ \inp -> runIter (mkI s) inp HasMore failX doneX
    -- TODO: probably makes sense to unroll this at least once
    f acc (MoreX k) s   = k s >>= \case
        res@(MoreX _) -> return (res,acc [])
        DoneX !(st',!e2) rest -> f (acc . (e2:)) (i0 st') rest
        FailX _ err -> throw $ IterFailure $ "unfoldConvStream: " ++ err
    f _acc _other _ = error "unfoldConvStream: other case arrived?"

iterXToStreamTrans :: Monad m => IterX s m (s -> [b]) -> StreamTransM m s [b]
iterXToStreamTrans iter = StreamTransM $ \s ->
  runIter iter s HasMore failX doneX >>= \case
    MoreX k     -> return (step k,[])
    DoneX f s'  -> finish s' f
    FailX _s err -> throw $ IterFailure
            $ "iterXToStreamTrans: " ++ err
  where
    finish s f = let f' = StreamTransM $ return . (f',) . f
                 in return (f',f s)
    step k = StreamTransM $ k >=> \case
        DoneX f s   -> finish s f
        MoreX k'    -> return (step k',[])
        FailX _s err -> throw $ IterFailure
            $ "iterXToStreamTrans: " ++ err

-- | create a transducer from a 'delimited stream'.
delimitG :: Monad m
         => IterX inp m st
         -> (st -> inp -> (Either st inp, [outp]))
         -> Transducer (StateT (DelState inp m st) (GenT outp m)) m inp outp
delimitG iter0 f = streamGM g s0
  where
    s0 = StartDelimiter
    g st e = case st of
        ProcState s -> g' [] s e
        StartDelimiter     -> runIter0 [] e
        ConsumeDelimiter k -> k e >>= procResult []

    g' outp0 s e = case f s e of
          (Left s',   outp) -> return (ProcState s', outp0 ++ outp)
          (Right nxt, outp) -> runIter0 (outp0++outp) nxt

    runIter0 o inp = runIter iter0 inp HasMore failX doneX >>= procResult o
    procResult outp res = case res of
              DoneX s' r  -> g' outp s' r
              MoreX k'    -> return $ (ConsumeDelimiter k', outp)
              FailX _ err -> throw $ IterFailure $ "delimitG: " ++ err
{-# INLINEABLE delimitG #-}

type DelState inp m st = DelStateD (inp -> m (ResultX inp m st)) st

data DelStateD i s =
    StartDelimiter
  | ConsumeDelimiter !i
  | ProcState !s

delimitN :: (IsSequence inp, Index inp ~ Int, Monad m)
         => IterX inp m Int
         -> Transducer (StateT (DelState inp m Int) (GenT inp m)) m inp inp
delimitN iter = delimitG iter f
  where
    f !n inp =
      let len = olength inp
      in if len <= n
          then (Left $! n-len, [inp])
          else case unsafeSplitAt n inp of
            (!h,t) -> (Right t,[h])
{-# INLINEABLE delimitN #-}

{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}

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
) where

import           IterX.Core
import           IterX.Exception

import           Control.Applicative
import           Control.Monad.State
import           Control.Exception (throw)
import           Data.Monoid

----------------------------------------------------------------

data ResultX s m r =
    DoneX r s
  | FailX s String
  | MoreX (s -> m (ResultX s m r))
    deriving (Functor)

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
cxtFailure cxt i = IterX $ \s st onF onD -> runIter i s st (cxtF onF) onD
  where
    cxtF onF s st err = onF s st (cxt ++ ": " ++ err)
{-# INLINE cxtFailure #-}

returnIter :: a -> IterX s m a
returnIter a = IterX $ \s st _ onD -> onD s st a
{-# NOINLINE returnIter #-}

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
           -> Transducer (GenT e2 (StateT (ResultX e1 m e2) m)) m e1 e2
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
                 -> Transducer (GenT e2 (StateT (ResultX e1 m (st,e2)) m)) m
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

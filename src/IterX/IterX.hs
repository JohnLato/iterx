{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}

{-# OPTIONS -Wall #-}
module IterX.IterX (
   ResultX(..)
  ,IterX(..)
  ,Failure
  ,Success

  ,failX
  ,doneX

  ,runIterX
  ,convStream
  ,unfoldConvStream
) where

import           IterX.Core
import           IterX.Exception

import           Control.Applicative
import           Control.Monad.State
import           Control.Exception (throw)

----------------------------------------------------------------

data ResultX m s r =
    DoneX r s
  | FailX s String
  | MoreX (s -> m (ResultX m s r))
    deriving (Functor)

newtype IterX m s a = IterX {
    runIter :: forall r.
               s
               -> Failure s m r
               -> Success s m a r
               -> m (ResultX m s r)}

type Failure s m r   = s -> String -> m (ResultX m s r)
type Success s m a r = s -> a -> m (ResultX m s r)

instance Functor (IterX m s) where
    {-# INLINE fmap #-}
    fmap   = mapIter

instance Applicative (IterX m s) where
    {-# INLINE pure #-}
    pure   = returnIter
    {-# INLINE (<*>) #-}
    (<*>)  = apIter

instance Monad (IterX m s) where
    {-# INLINE return #-}
    return = returnIter
    {-# INLINE (>>=) #-}
    (>>=)  = bindIter
    fail   = failIter

failX :: Monad m => s -> String -> m (ResultX m s a)
failX s err = return $ FailX s err

doneX :: Monad m => s -> a -> m (ResultX m s a)
doneX s a   = return $ DoneX a s

returnIter :: a -> IterX m s a
returnIter a = IterX $ \s _ onD -> onD s a
{-# NOINLINE returnIter #-}

bindIter :: IterX m s a -> (a -> IterX m s b) -> IterX m s b
bindIter m f = IterX $ \s onF onD -> runIter m s onF
    $ \s' a -> runIter (f a) s' onF onD
{-# INLINE bindIter #-}

failIter :: String -> IterX m s a
failIter err = IterX $ \s onF _ -> onF s err
{-# INLINE failIter #-}

mapIter :: (a -> b) -> IterX m s a -> IterX m s b
mapIter f m = IterX $ \s onF onD -> runIter m s onF $ \s' a -> onD s' (f a)
{-# INLINE mapIter #-}

apIter :: IterX m s (a -> b) -> IterX m s a -> IterX m s b
apIter f a = do
    f' <- f
    a' <- a
    return (f' a')
{-# INLINE apIter #-}

----------------------------------------------------------------

-- run an iteratee with the given producer.  Extra input after the iteratee
-- completes is discarded.
runIterX :: (Monad m)
         => Producer (StateT (ResultX m s a) m) s
         -> IterX m s a
         -> m (ResultX m s a)
runIterX gen i = do
    let s0 = MoreX $ \s -> runIter i s failX doneX
    foldG f s0 gen
  where
    f (MoreX k) s   = k s
    f (DoneX _ _) _ = throw (TerminateEarly "runIterX")
    f r _ = return r

----------------------------------------------------------------

-- | Create a 'Transducer' from an 'IterX'
convStream :: Monad m
           => IterX m e1 e2
           -> Transducer (GenT e2 (StateT (ResultX m e1 e2) m)) m e1 e2
convStream i = streamGM (f id) i0
  where
    i0 = MoreX $ \inp -> runIter i inp failX doneX
    f acc (MoreX k) s =  k s >>= \case
        res@(MoreX _) -> return (res,acc [])
        -- TODO: probably makes sense to unroll this at least once
        DoneX e2 rest -> f (acc . (e2:)) i0 rest
        FailX _ err -> throw $ IterFailure $ "convStream: " ++ err
    f _acc _other _ = error "convStream: other case arrived?"

-- | Create a 'Transducer' from an 'IterX' generating function
unfoldConvStream :: Monad m
                 => (st -> IterX m e1 (st,e2))
                 -> st
                 -> Transducer (GenT e2 (StateT (ResultX m e1 (st,e2)) m)) m
                      e1 e2
unfoldConvStream mkI st0 = streamGM (f id) (i0 st0)
  where
    i0 s = MoreX $ \inp -> runIter (mkI s) inp failX doneX
    -- TODO: probably makes sense to unroll this at least once
    f acc (MoreX k) s   = k s >>= \case
        res@(MoreX _) -> return (res,acc [])
        DoneX !(st',!e2) rest -> f (acc . (e2:)) (i0 st') rest
        FailX _ err -> throw $ IterFailure $ "unfoldConvStream: " ++ err
    f _acc _other _ = error "unfoldConvStream: other case arrived?"

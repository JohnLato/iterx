{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

module IterX.Parser (
  IterX.Parser.take
, IterX.Parser.drop
, match

, tryHead
, IterX.Parser.head
, pElem
, matchElem

, splitChunkAt
, foldI
, delimitI

) where

import IterX.IterX
import IterX.Unsafe

import Control.Monad.Trans (lift)
import Data.MonoTraversable
import Data.Monoid
import Data.Sequences (IsSequence)
import qualified Data.Sequences as S

forceAppend :: (Monoid s, Monad m) => IterX s m ()
forceAppend = IterX $ \s0 st0 onF onD -> case st0 of
    EOF     -> onF s0 st0 "unexpected EOF"
    HasMore -> return $ MoreX $ \s -> onD (s0 <> s) st0 ()

reqSize' :: (MonoFoldableMonoid s, Monad m)
         => Int -> s -> Status -> Failure s m r -> Success s m s r
         -> m (ResultX s m r)
reqSize' !n0 s st0 onF0 onD0 =
    runIter (forceAppend >> go n0) s st0 onF0 onD0
  where
    go !n = IterX $ \s st onF onD ->
        if olength s >= n
        then onD s st s
        else runIter (forceAppend >> go n) s st onF onD
-- we don't want this inline'd, but we do want it type-specialized
{-# INLINE reqSize' #-}

reqSize :: (MonoFoldableMonoid s, Monad m) => Int -> IterX s m s
reqSize !n = IterX $ \s st onF onD ->
    if olength s >= n
    then onD s st s
    else reqSize' n s st onF onD
{-# INLINE reqSize #-}

-- TODO: this can possibly be optimized by re-writing it similar to reqSize.
-- but when I tried it, didn't make much difference
{-# INLINE foldI #-}
foldI n f fs0 = go (max n 0) fs0
  where
    go !n fs = do
      t <- inputReady
      if t then do
        s <- get
        if olength s >= n
        then let !(h,t) = unsafeSplitAt n s in do
              put t
              lift $ f fs h
        else do
              fs' <- lift $ f fs s
              put mempty
              go (n-olength s) fs'
      else return fs

delimitI :: (Monad m, MonoFoldableMonoid s, IsSequence s, Int ~ S.Index s)
         => Int -> IterX s m a -> IterX s m a
delimitI n0 i0 = IterX $ \s st onF onD -> let !n = max 0 n0 in
    if olength s >= n
    then let !(h,!t) = unsafeSplitAt n s in do
              runIter i0 h EOF onF onD >>= \case
                  DoneX a s   -> return $ DoneX a (s <> t)
                  MoreX k     -> return $ FailX t "delimitI: not enough data!"
                  FailX s err -> return $ FailX s err
    else runIter i0 s HasMore onF onD >>= \case
        MoreX k -> return $ MoreX (mkK (n-olength s) k)
        res -> return res
  where
    mkK n k s =
      if olength s >= n
      then let !(h,!t) = unsafeSplitAt n s
           in k h >>= \case
                  DoneX a s' -> return $ DoneX a (s' <> t)
                  MoreX k    -> return $ FailX t "delimitI: not enough data!"
                  FailX s' e -> return $ FailX s' e
      else k s >>= \case
          MoreX k' -> return $ MoreX (mkK (n-olength s) k')
          res -> return res
{-# INLINE delimitI #-}

delimitI' !n0 r0 s st0 onF0 onD0 = undefined
{-
    runIter (go r0 n0) s st0 onF0 onD0
  where
    go i !n = IterX $ \s st onF onD ->
        if olength s >= n
        then onD s st s
        else runIter (forceAppend >> go n) s st onF onD
-- we don't want this inline'd, but we do want it type-specialized
-- {-# INLINE delimitI' #-}
-}


inputReady :: (MonoFoldableMonoid s, Monad m) => IterX s m Bool
inputReady = IterX $ \s st _onF onD ->
    if
       | not (onull s) -> onD s st True
       | st == EOF     -> onD s st False
       | otherwise     -> return $ MoreX $ \s' ->
          if onull s' then onD s' st False else onD s' st True
{-# INLINE inputReady #-}

-- get the current stream
get :: IterX s m s
get = IterX $ \i st _ onD -> onD i st i

-- put an element into the stream.
-- this is *very* unsafe.
put :: s -> IterX s m ()
put s = IterX $ \_ st _ onD -> onD s st ()

-----------------------------------------------------------------

tryHead :: (MonoFoldableMonoid s, IsSequence s, Monad m)
        => IterX s m (Maybe (Element s))
tryHead = do
  isReady <- inputReady
  if isReady then do
      x <- get
      case unsafeUncons x of
        (!h,!t) -> put t >> return (Just h)
    else return Nothing
{-# INLINE tryHead #-}

head :: (MonoFoldableMonoid s, IsSequence s, Monad m)
     => IterX s m (Element s)
head = pElem (const True)
{-# INLINE head #-}

pElem :: (MonoFoldableMonoid s, IsSequence s, Monad m)
          => (Element s -> Bool) -> IterX s m (Element s)
pElem p = do
  isReady <- inputReady
  if isReady then do
      x <- get
      case unsafeUncons x of
        (!h,!t) -> put t >> if p h then return h
                     else fail "IterX.pElem: match failed"
    else fail "IterX.pElem: no input available"
{-# INLINE pElem #-}

matchElem :: (MonoFoldableMonoid s, IsSequence s, Monad m, Eq (Element s))
     => Element s -> IterX s m (Element s)
matchElem = pElem . (==)
{-# INLINE matchElem #-}
-----------------------------------------------------------------

-- | Consume @n@ elements of input, but succeed only if the predicate
-- returns 'True'.

-- this function doesn't stream well, and should only be used for relatively
-- small amounts to take.
takeWith :: (S.Index s ~ Int, IsSequence s, MonoFoldableMonoid s, Monad m)
         => Int -> (s -> Bool) -> IterX s m s
takeWith n0 p = do
  let n = max n0 0
  s <- reqSize n
  let !(!h,!t) = unsafeSplitAt n s
  if p h
    then put t >> return h
    else fail "takeWith: predicate failed"
{-# INLINE takeWith #-}

-- | consume exactly @n@ elements of input
take :: (S.Index s ~ Int, IsSequence s, MonoFoldableMonoid s, Monad m)
         => Int -> IterX s m s
take n = cxtFailure "take" $ takeWith n (const True)
{-# INLINE take #-}

-- | drop up to @n@ elements of input, however the start of
-- the last chunk is also returned.
splitChunkAt :: (S.Index s ~ Int, IsSequence s, MonoFoldableMonoid s, Monad m)
             => Int -> IterX s m s
splitChunkAt n0 = do
  s <- get
  let !n' = max n0 0
      !curlen = olength s
  if curlen >= n' then let !(h,!t) = unsafeSplitAt n' s in put t >> return h
    else splitLoop n'
{-# INLINE splitChunkAt #-}

-- | drop up to @n@ elements of input.  This will not fail.
drop :: (S.Index s ~ Int, IsSequence s, MonoFoldableMonoid s, Monad m)
         => Int -> IterX s m ()
drop n0 = do
  s <- get
  let !n' = max n0 0
      !curlen = olength s
  if curlen >= n' then let !(_,!t) = unsafeSplitAt n' s in put t
    else dropLoop n'
{-# INLINE drop #-}

dropLoop :: (S.Index s ~ Int, IsSequence s, MonoFoldableMonoid s, Monad m)
         => Int -> IterX s m ()
dropLoop = loop
  where
    loop n = do
      t <- inputReady
      if t then do
          s <- get
          let !curlen = olength s
          if curlen >= n then let !(_,!t) = unsafeSplitAt n s in put t
            else put mempty >> (loop $! n - curlen)
        else return ()
{-# INLINE dropLoop #-}

splitLoop :: (S.Index s ~ Int, IsSequence s, MonoFoldableMonoid s, Monad m)
         => Int -> IterX s m s
splitLoop = loop
  where
    loop n = do
      t <- inputReady
      if t then do
          s <- get
          let !curlen = olength s
          if curlen >= n then let !(h,!t) = unsafeSplitAt n s in put t >> return h
            else put mempty >> (loop $! n-curlen)
        else return mempty
{-# INLINE splitLoop #-}


-- | parse a sequence of elements that exactly matches @s@.
--
-- Similar to 'Data.Attoparsec.string'
match :: (S.Index s ~ Int, IsSequence s, MonoFoldableMonoid s, Eq s, Monad m)
         => s -> IterX s m s
match s = cxtFailure "match" $ takeWith (olength s) (== s)
{-# INLINE match #-}

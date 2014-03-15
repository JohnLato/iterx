{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Unfold (
UnfoldM(..),
UnfoldStep(..),
unfoldDone,
unfoldIdM,

Unfold2(..),
unfold2Vec,

-- * some common unfoldings
unfoldList,
unfoldVec,
unfoldVec2,

uReplicate,
unfoldEmpty,
) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Stream as S
import Control.Monad (liftM)

-- -----------------------------------------
-- Unfoldings

data UnfoldM m full a where
    UnfoldM :: (full -> m s) -> (s -> m (UnfoldStep s a)) -> UnfoldM m full a

data UnfoldStep s a = UnfoldDone | UnfoldStep !a !s

instance Functor (UnfoldStep s) where
    fmap f (UnfoldStep a s) = UnfoldStep (f a) s
    fmap _ UnfoldDone = UnfoldDone

data Unfold2 m a where
    Unfold2 :: s -> (s -> m (UnfoldStep s a)) -> Unfold2 m a

instance Monad m => Functor (Unfold2 m) where
    {-# INLINE fmap #-}
    fmap f (Unfold2 s uf) = Unfold2 s ((liftM . fmap) f . uf)

unfoldDone :: Monad m => m (UnfoldStep a s)
unfoldDone = return UnfoldDone

{-# INLINE [1] unfoldIdM #-}
unfoldIdM :: Monad m => UnfoldM m a a
unfoldIdM = UnfoldM (return . Just) $ \x -> case x of
    Just a  -> return $ UnfoldStep a Nothing
    Nothing -> unfoldDone

{-# INLINE uReplicate #-}
uReplicate :: Monad m => Int -> a -> UnfoldM m () a
uReplicate n a = UnfoldM (const $ return 0) $ \n' -> if n' < n
    then return $ UnfoldStep a (n'+1)
    else unfoldDone

{-# INLINE unfoldEmpty #-}
unfoldEmpty :: Monad m => UnfoldM m () a
unfoldEmpty = UnfoldM (const $ return ()) (const unfoldDone)

-- this currently performs much better than the closure-based unfolding
--
-- TODO: define something generic using mono-traversable, and maybe
-- substitute other variants via RULEs.
{-# INLINE unfoldVec #-}
unfoldVec :: (G.Vector v a, Monad m) => UnfoldM m (v a) a
unfoldVec = UnfoldM mkS f
  where
    {-# INLINE mkS #-}
    mkS v = return v
    -- using stream causes compilation to blow up.  urk.
    {-# INLINE f #-}
    f v | not (G.null v) = return $ UnfoldStep (G.unsafeHead v) (G.unsafeDrop 1 v)
        | otherwise = unfoldDone

{-# INLINE [1] unfold2Vec #-}
unfold2Vec :: (G.Vector v a, Monad m) => v a -> Unfold2 m a
unfold2Vec vec = Unfold2 0 f
  where
    l = G.length vec
    {-# INLINE [0] f #-}
    f ix | (ix < l)  = return $ UnfoldStep (G.unsafeIndex vec ix) (ix+1)
         | otherwise = unfoldDone

-------------------------------------------------------
newtype Stepper b = Stepper { _unStepper :: Maybe (b, Stepper b) }

{-# INLINE unfoldVec2 #-}
unfoldVec2 :: (G.Vector v a, Monad m) => UnfoldM m (v a) a
unfoldVec2 = UnfoldM mkS loop
  where
    {-# INLINE [0] mkS #-}
    mkS v =
        let f i | i < G.length v =
                    Stepper $ Just (G.unsafeIndex v i, f (i+1))
                | otherwise = (Stepper Nothing)
        in return $ f 0
    loop (Stepper this) = case this of
        Just (a,b) -> return $ UnfoldStep a b
        Nothing    -> unfoldDone
---------------------------------------------------------

{-# INLINE unfoldList #-}
unfoldList :: Monad m => UnfoldM m [a] a
unfoldList = UnfoldM return f
  where
    f (x:xs) = return $ UnfoldStep x xs
    f []     = unfoldDone

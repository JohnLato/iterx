{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS -Wall #-}
module IterX.Fusion.Unfold (
UnfoldM(..),
UnfoldStep(..),
unfoldIdM,

-- * some common unfoldings
unfoldList,
unfoldVec,
unfoldVec2,

uReplicate,
) where

import qualified Data.Vector.Generic as G

-- -----------------------------------------
-- Unfoldings

data UnfoldM m full a where
    UnfoldM :: (full -> m s) -> (s -> m (UnfoldStep a s)) -> UnfoldM m full a
    SUnfoldM :: t -> (t -> full -> s) -> (s -> m (Either t (a,s))) -> UnfoldM m full a

data UnfoldStep a s = UnfoldDone | UnfoldStep !a !s

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

-- this currently performs much better than the closure-based unfolding
-- except on the transducer tests
--
-- TODO: define something generic using mono-traversable, and maybe
-- substitute other variants via RULEs.
{-# INLINE unfoldVec #-}
unfoldVec :: (G.Vector v a, Monad m) => UnfoldM m (v a) a
unfoldVec = UnfoldM mkS f
  where
    {-# INLINE mkS #-}
    mkS v = return v
    {-# INLINE f #-}
    f v | not (G.null v) = let !x = G.unsafeHead v
                           in return $ UnfoldStep x (G.unsafeDrop 1 v)
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

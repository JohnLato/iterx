{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}

module IterX.Unsafe (
  unsafeUncons
, unsafeSplitAt
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.Vector.Generic as V
import qualified Data.Vector as VV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS 
import Data.Word  (Word8)
import Data.Maybe (fromMaybe)

import Data.MonoTraversable
import Data.Monoid
import Data.Sequences (IsSequence)
import qualified Data.Sequences as S

unsafeUncons :: IsSequence s => s -> (Element s, s)
unsafeUncons = fromMaybe (error "unsafeUncons: empty input") . S.uncons
{-# NOINLINE[2] unsafeUncons #-}

{-# RULES "unsafeUncons/bytestring" unsafeUncons = bsUncons #-}
{-# RULES "unsafeUncons/vector"     unsafeUncons = (vUncons :: VV.Vector a -> (a, VV.Vector a)) #-}
{-# RULES "unsafeUncons/uvector"    forall (v :: U.Unbox a => U.Vector a). unsafeUncons v = vUncons v #-}
{-# RULES "unsafeUncons/svector"    forall (v :: VS.Storable a => VS.Vector a). unsafeUncons v = vUncons v #-}

bsUncons :: B.ByteString -> (Word8, B.ByteString)
bsUncons b = (B.head b, B.drop 1 b)
{-# INLINE bsUncons #-}

vUncons :: V.Vector v a => v a -> (a, v a)
vUncons v = (V.unsafeHead v, V.unsafeDrop 1 v)
{-# INLINE vUncons #-}

unsafeSplitAt :: (IsSequence s, S.Index s ~ Int) => Int -> s -> (s, s)
unsafeSplitAt = S.splitAt
{-# NOINLINE[2] unsafeSplitAt #-}
{-# RULES "unsafeSplitAt/bytestring" unsafeSplitAt = bsSplitAt #-}
{-# RULES "unsafeSplitAt/vector"     forall n (v :: V.Vector v a => v a). unsafeSplitAt n v = vSplitAt n v #-}

bsSplitAt :: Int -> B.ByteString -> (B.ByteString,B.ByteString)
bsSplitAt n b = (B.unsafeTake n b, B.unsafeDrop n b)
{-# INLINE bsSplitAt #-}

vSplitAt :: V.Vector v a => Int -> v a -> (v a, v a)
vSplitAt n v = (V.unsafeTake n v, V.unsafeDrop n v)
{-# INLINE vSplitAt #-}


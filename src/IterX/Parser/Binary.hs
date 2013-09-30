{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}
module IterX.Parser.Binary (
  getWord8

, getWord16le
, getWord24le
, getWord32le
, getWord64le

, getWord16be
, getWord24be
, getWord32be
, getWord64be
) where

import IterX.IterX
import IterX.Parser as IterX

import Control.Applicative
import Data.MonoTraversable
import Data.Sequences

import Data.Bits
import Data.Word
import Data.Word.Word24

packBE :: (Bits b, Num b, MonoFoldable s, Element s ~ Word8) => s -> b
packBE = ofoldl' (\acc sml -> (acc `shiftL` 1) .|. fromIntegral sml) 0
{-# INLINE packBE #-}

packLE :: (Bits b, Num b, MonoFoldable s, Element s ~ Word8) => s -> b
packLE = ofoldr  (\sml acc -> fromIntegral sml .|. (acc `shiftL` 1)) 0
{-# INLINE packLE #-}

getWordn :: forall s m b. (Bits b, Element s ~ Word8, MonoFoldableMonoid s
                          ,IsSequence s, Index s ~ Int, Monad m)
         => (s -> b) -> IterX s m b
getWordn packer = packer <$> IterX.take numBytes
  where
    numBytes = bitSize (undefined :: b) `quot` 8
{-# INLINE getWordn #-}

getWord8 :: forall s m. (Element s ~ Word8, MonoFoldableMonoid s
                        ,IsSequence s, Index s ~ Int, Monad m)
         => IterX s m Word8
getWord8 = IterX.head

getWord16be :: forall s m. (Element s ~ Word8, MonoFoldableMonoid s
                         ,IsSequence s, Index s ~ Int, Monad m)
            => IterX s m Word16
getWord16be = getWordn packBE
{-# INLINE getWord16be #-}

getWord24be :: forall s m. (Element s ~ Word8, MonoFoldableMonoid s
                         ,IsSequence s, Index s ~ Int, Monad m)
            => IterX s m Word24
getWord24be = getWordn packBE
{-# INLINE getWord24be #-}

getWord32be :: forall s m. (Element s ~ Word8, MonoFoldableMonoid s
                         ,IsSequence s, Index s ~ Int, Monad m)
            => IterX s m Word32
getWord32be = getWordn packBE
{-# INLINE getWord32be #-}

getWord64be :: forall s m. (Element s ~ Word8, MonoFoldableMonoid s
                         ,IsSequence s, Index s ~ Int, Monad m)
            => IterX s m Word64
getWord64be = getWordn packBE
{-# INLINE getWord64be #-}

getWord16le :: forall s m. (Element s ~ Word8, MonoFoldableMonoid s
                         ,IsSequence s, Index s ~ Int, Monad m)
            => IterX s m Word16
getWord16le = getWordn packLE
{-# INLINE getWord16le #-}

getWord24le :: forall s m. (Element s ~ Word8, MonoFoldableMonoid s
                         ,IsSequence s, Index s ~ Int, Monad m)
            => IterX s m Word24
getWord24le = getWordn packLE
{-# INLINE getWord24le #-}

getWord32le :: forall s m. (Element s ~ Word8, MonoFoldableMonoid s
                         ,IsSequence s, Index s ~ Int, Monad m)
            => IterX s m Word32
getWord32le = getWordn packLE
{-# INLINE getWord32le #-}

getWord64le :: forall s m. (Element s ~ Word8, MonoFoldableMonoid s
                         ,IsSequence s, Index s ~ Int, Monad m)
            => IterX s m Word64
getWord64le = getWordn packLE
{-# INLINE getWord64le #-}

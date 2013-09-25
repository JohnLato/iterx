{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wall #-}

-- |Monadic Iteratees:
-- incremental input parsers, processors and transformers
--
-- Support for IO enumerators

module IterX.ReadableChunk (
  ReadableChunk (..)
, SizedS (..)
)
where

import Prelude hiding (head, tail, dropWhile, length, splitAt )

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Word
import Foreign.C
import Foreign.ForeignPtr  (withForeignPtr, mallocForeignPtrBytes)
import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

data SizedS s = SizedS {-# UNPACK #-} !Int s deriving Functor

-- |Class of streams which can be filled from a 'Ptr'.  Typically these
-- are streams which can be read from a file, @Handle@, or similar resource.
--
-- instances must implement 'readFromPtr' and optionally 'fillFromCallback'.
-- if available, 'fillFromCallback' results in less copying.
class (Monoid s) => ReadableChunk s where
  type El s :: *
  readFromPtr :: Ptr (El s)
              -> Int -- ^ The pointer must not be used after @readFromPtr@ completes.
              -> IO s -- ^ The Int parameter is the length of the data in *bytes*.
  fillFromCallback :: Int -> (Ptr (El s) -> Int -> IO Int) -> IO (SizedS s)
  fillFromCallback sz cb = allocaBytes sz $ \p -> do
          nRead <- cb p sz
          SizedS nRead <$> readFromPtr p nRead
  empty :: s
  empty = mempty

{-# DEPRECATED readFromPtr "use fillFromCallback" #-}

instance ReadableChunk [Char] where
  type El [Char] = Word8
  readFromPtr buf l = peekCAStringLen (castPtr buf, l)

instance ReadableChunk [Word8] where
  type El [Word8] = Word8
  readFromPtr buf l = peekArray l buf
instance ReadableChunk [Word16] where
  type El [Word16] = Word16
  readFromPtr buf l = peekArray l buf
instance ReadableChunk [Word32] where
  type El [Word32] = Word32
  readFromPtr buf l = peekArray l buf
instance ReadableChunk [Word] where
  type El [Word] = Word
  readFromPtr buf l = peekArray l buf

instance ReadableChunk B.ByteString where
  type El B.ByteString = Word8
  readFromPtr buf l = B.packCStringLen (castPtr buf, l)
  fillFromCallback sz cb = do
      fp <- mallocForeignPtrBytes sz
      numFill <- withForeignPtr fp $ \p -> cb p sz
      return $ SizedS numFill $! B.PS fp 0 numFill

instance ReadableChunk L.ByteString where
  type El L.ByteString = Word8
  readFromPtr buf l = return . L.fromChunks . (:[]) =<< readFromPtr buf l
  fillFromCallback sz cb = fmap (L.fromChunks . (:[])) <$> fillFromCallback sz cb

{-# LANGUAGE TupleSections #-}

module IterX.Attoparsec (
  attoToIterX
) where

import           IterX.Exception
import           IterX.IterX

import           Control.Exception (throw)
import           Data.Attoparsec
import           Data.ByteString (ByteString)

attoToIterX :: Monad m => Parser a -> IterX ByteString m a
attoToIterX p = IterX $ \i st onF onS ->
    let step (Done s r) = onS s st r
        step (Fail s cxt err) = onF s st (unlines (cxt ++ [err]))
        step (Partial k) = return . MoreX $ step . k
    in step (parse p i)

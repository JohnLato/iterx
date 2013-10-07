{-# LANGUAGE NoMonomorphismRestriction #-}
module IterX.StreamTrans (
  StreamTrans(..)
, StreamTransM(..)
, transG
, transGM
) where

import IterX.Core
import Control.Monad.State

newtype StreamTrans a b = StreamTrans { unST :: a -> (StreamTrans a b, b) }

newtype StreamTransM m a b =
    StreamTransM { unSTM :: a -> m (StreamTransM m a b, b) }

transG :: Monad m
       => StreamTrans e1 [e2]
       -> Transducer (StateT (StreamTrans e1 [e2]) (GenT e2 m)) m e1 e2
transG = streamG unST

transGM :: Monad m
        => StreamTransM m e1 [e2]
        -> Transducer (StateT (StreamTransM m e1 [e2]) (GenT e2 m)) m e1 e2
transGM = streamGM unSTM

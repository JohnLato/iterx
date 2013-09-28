module IterX.StreamTrans (
  StreamTrans(..)
, transG
) where

import IterX.Core
import Control.Monad.State

newtype StreamTrans a b = StreamTrans { unST :: a -> (StreamTrans a b, b) }

transG :: Monad m
       => StreamTrans e1 [e2]
       -> Transducer (GenT e2 (StateT (StreamTrans e1 [e2]) m)) m e1 e2
transG = streamG unST


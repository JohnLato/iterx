module Bar where

import Prelude hiding ((.))
import Control.Category
import IterX.Stream
import IterX.Core
import Data.Vector as V

v1 :: V.Vector Int
v1 = V.enumFromN 1 10

modV1 :: Monad m => Stream m Int Int
modV1 = mapT (+1)

modV2 :: Monad m => Stream m Int Int
modV2 = mapT (+1) . mapT (*2)

modV3 :: Monad m => Stream m Int Int
modV3 = mapT (*2) . filterT even . mapT (+1)


-- these all fuse nicely.  Very nicely indeed.
doIt2 :: IO (V.Vector Int)
doIt2 = runStream modV2 v1

doIt3 :: IO (V.Vector Int)
doIt3 = runStream modV3 v1

doIt3b :: IO (V.Vector Int)
doIt3b = fromStream (V.length v1) $  modV3 . toStream v1

doIt4 :: IO (V.Vector Int)
doIt4 = withChunk v1 modV2 >>= flip withChunk modV3 >>= \x -> withChunk x modV2

-------------------------------------------------------------
gen1 :: Monad m => Producer m (V.Vector Int)
gen1 = yieldList [v1,v1]

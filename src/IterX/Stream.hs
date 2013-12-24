{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module IterX.Stream where

import Prelude hiding (id, (.))
import IterX.Core

import Control.Applicative
import Control.Category
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Foldable
import Data.Traversable
import Unsafe.Coerce
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- type Blarg s t a b = forall f. (a -> f b) -> s -> f t
-- type PBlarg p s t a b = forall f. p a (f b) -> s -> f t

data Step s o =
    Val s o
  | Skip s
  | End
  deriving (Functor, Foldable, Traversable)

-- what if the function is over some type of profunctor?  Would it still
-- fuse super-nicely?  Would need something like...

-- class Profunctor p => Furk p where
    -- furk :: p a b -> a -> b

-- ultimate goal: be able to freely combine Streams with Iteratees.
data Stream m i o where
    Stream :: (s -> i -> m (Step s o)) -> s -> Stream m i o

instance Monad m => Category (Stream m) where
    {-# INLINE id #-}
    id = idStream
    (.) = cmpStream

data Any

aNY :: Any
aNY = undefined

-- toStream/fromStream is probably better than runStream, because it's simpler
-- to make it work for conversions between different types (i.e. vector/list)

{-# INLINE [1] toStream #-}
toStream :: Monad m => V.Vector i -> Stream m Any i
toStream v = Stream loop 0
  where
    {-# INLINE [0] loop #-}
    loop i _ | i < V.length v = return (Val (i+1) (V.unsafeIndex v i))
             | otherwise = return End

{-# INLINE [1] fromStream  #-}
fromStream :: MonadIO m => Int -> Stream m Any o -> m (V.Vector o)
fromStream sz str = do
    v <- liftIO $ VM.unsafeNew sz
    let put i !x = liftIO (VM.unsafeWrite v i x) >> return (i+1)
    case foldMT put 0 . str of
        (Stream f s0) -> do
            let go o s = do
                    f s aNY >>= \case
                      Val s' o' -> go o' s'
                      Skip s'   -> go o  s'
                      End       -> return o
            n' <- go 0 s0
            liftIO $ V.unsafeFreeze $ VM.unsafeSlice 0 n' v

{-# INLINE [1] runStream #-}
runStream :: MonadIO m => Stream m i o -> V.Vector i -> m (V.Vector o)
runStream str inp = do
    v <- liftIO $ VM.unsafeNew (V.length inp)
    let put i !x = liftIO (VM.unsafeWrite v i x) >> return (i+1)
    case foldMT put 0 . str of
        (Stream f s0) -> do
            let go o s i | i < V.length inp = do
                    f s (V.unsafeIndex inp i) >>= \case
                      Val s' o' -> go o' s' (i+1)
                      Skip s'   -> go o  s' (i+1)
                      End       -> return o
                         | otherwise = return o
            n' <- go 0 s0 0
            liftIO $ V.unsafeFreeze $ VM.unsafeSlice 0 n' v

{-# INLINE [1] idStream #-}
idStream :: Monad m => Stream m i i
idStream = Stream loop ()
  where
    {-# INLINE [0] loop #-}
    loop !() i = return (Val () i)

{-# INLINE [1] cmpStream #-}
cmpStream :: Monad m => Stream m b c -> Stream m a b -> Stream m a c
cmpStream (Stream rF rs0) (Stream lF ls0) = Stream loop (ls0,rs0)
  where
    {-# INLINE [0] loop #-}
    loop (ls,rs) i0 = do
        lRes <- lF ls i0
        case lRes of
          Val ls' i1 -> do
              rF rs i1 >>= \case
                  Val rs' o -> return (Val (ls',rs') o)
                  Skip rs'  -> return (Skip (ls',rs'))
                  End       -> return End
          Skip ls' -> return (Skip (ls',rs))
          End      -> return End

{-# INLINE [1] mapMT #-}
mapMT :: Monad m => (i -> m o) -> Stream m i o
mapMT f = Stream loop ()
  where
    {-# INLINE [0] loop #-}
    loop !() i = liftM (Val ()) (f i)

{-# INLINE [1] mapT #-}
mapT :: Monad m => (i -> o) -> Stream m i o
mapT f = mapMT (return . f)

{-# INLINE [1] filterT #-}
filterT :: Monad m => (i -> Bool) -> Stream m i i
filterT p = Stream loop ()
  where
    {-# INLINE [0] loop #-}
    loop !() i = case p i of
        True  -> return (Val () i)
        False -> return (Skip ())

{-# INLINE [1] foldMT #-}
foldMT :: Monad m => (s -> i -> m s) -> s -> Stream m i s
foldMT f s0 = Stream loop s0
  where
    {-# INLINE [0] loop #-}
    loop s i = do
        o <- f s i
        return (Val o o)

--------------------------------------------------------

{-# INLINE [1] mkTransducer #-}
mkTransducer :: Monad m => Stream m i o -> (forall s. Producer (StateT s (GenT o m)) i) -> Producer m o
mkTransducer (Stream f s0) gen = foldG f' s0 gen >> return ()
  where
    {-# INLINE [0] f' #-}
    f' s i = lift (f s i) >>= \case
        Val s' o -> yield o >> return s'
        Skip s'  -> return s'
        End      -> return s

-- I want to run a stream function over a chunk...
-- Stream m (V.Vector a) a
-- isn't really good enough, because that will build up new chunks in the state
-- at each cycle...
-- 
-- this is where I need a bundle type, or a monoid output.
-- What about doing something like
--
-- withChunk :: Vector a -> Stream m a b -> m (Vector b)
--
-- and using RULES to fuse
--
-- withChunk va sf1 >>= \vb -> withChunk vb sf2 -->
-- withChunk va (sf1 . sf2)

-- seems to work, I guess...

-- we can't inline too early, or we won't be able to match the flip'd rule.
{-# INLINE [0] withChunk #-}
withChunk :: MonadIO m => V.Vector a -> Stream m a b -> m (V.Vector b)
withChunk v sf = fromStream (V.length v) (sf . toStream v)

{-# RULES "withChunk/withChunk" forall v sf1 sf2. withChunk v sf1 >>= \vb -> withChunk vb sf2 = withChunk v (sf2 . sf1) #-}

{-# RULES "withChunk/flip withChunk" forall v sf1 sf2. withChunk v sf1 >>= flip withChunk sf2 = withChunk v (sf2 . sf1) #-}

{- I can write this.  I need to pretty much inline toStream/fromStream though,
 - in order to get the state to fold over properly.
 -
 - then to make it work with chunks (e.g. SIMD) would be a nasy duplication...
 - what I need is a way to fold a stream and get the hidden state.
distribute :: Stream m a b -> Stream m (V.Vector a) (V.Vector b)
distribute (Stream f s0) = Stream loop s0
  where
    loop s v = inner v s 0
    inner v s ix
      | ix < V.length v =
-}

-- how do I mix this with iteratees?  I need something like unfoldConvStream
-- and also delimitG

-- as an alternative, instead of thinking of streams think of loops. We have a
-- bunch of nested loops, and we want to fuse them together whenever we can.
-- e.g.
--
-- for x in y: do
--   for z in x: do
--   for z in x: do
--
-- in this case, the two inner loops can be combined. We don't need to know
-- anything else, and we'll never try to cross loop bounds!


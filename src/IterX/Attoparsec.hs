{-# LANGUAGE TupleSections #-}

module IterX.Attoparsec (
  attoToIterX
, attoToStreamTrans
) where

import           IterX.Exception
import           IterX.IterX
import           IterX.StreamTrans

import           Control.Exception (throw)
import           Data.Attoparsec
import           Data.ByteString (ByteString)

attoToIterX :: Monad m => Parser a -> IterX m ByteString a
attoToIterX p = IterX $ \i onF onS ->
    let step (Done s r) = onS s r
        step (Fail s cxt err) = onF s (unlines (cxt ++ [err]))
        step (Partial k) = return . MoreX $ step . k
    in step (parse p i)

attoToStreamTrans :: Parser (ByteString -> [b]) -> StreamTrans ByteString [b]
attoToStreamTrans p = StreamTrans $ \i -> case parse p i of
    Partial k -> (step k,[])
    Done s f  -> finish s f
    Fail s cxt err -> throw $ IterFailure
            $ "attoToStreamTrans: " ++ unlines (cxt ++ [err])
  where
    finish s f = let f' = StreamTrans $ (f',) . f
                 in (f',f s)
    step k = StreamTrans $ \i -> case k i of
        Done s f       -> finish s f
        Partial k'     -> (step k',[])
        Fail s cxt err -> throw $ IterFailure
            $ "attoToStreamTrans: " ++ unlines (cxt ++ [err])

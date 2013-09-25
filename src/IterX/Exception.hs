{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable        #-}

{-# OPTIONS -Wall #-}

module IterX.Exception (
   IterException

  ,TerminateEarly(..)
  ,isTerminateEarly
  ,terminateEarlyHandler

  ,IterFailure(..)
  ,isIterFailure
) where

import Control.Exception
import Data.Data

-- and all inheritants are descendents of 'IterException'.
data IterException = forall e . Exception e => IterException e
  deriving (Typeable)

instance Show IterException where
  show (IterException e) = show e

instance Exception IterException

iExceptionToException :: Exception e => e -> SomeException
iExceptionToException = toException . IterException

iExceptionFromException :: Exception e => SomeException -> Maybe e
iExceptionFromException x = do
  IterException a <- fromException x
  cast a

class Exception e => IException e where
  toIterException :: e -> IterException
  toIterException = IterException
  fromIterException :: IterException -> Maybe e
  fromIterException = fromException . toException

-- | The consumer has indicated that it's done, and the generator should
-- terminate (further values will have no effect)
data TerminateEarly = TerminateEarly String
  deriving (Typeable, Show)

instance Exception TerminateEarly where
  toException   = iExceptionToException
  fromException = iExceptionFromException

instance IException TerminateEarly

isTerminateEarly :: Exception e => e -> Bool
isTerminateEarly e = case fromException $ toException e of
    Just (TerminateEarly _) -> True
    _                       -> False

terminateEarlyHandler :: Monad m => TerminateEarly -> m Bool
terminateEarlyHandler _ = return False

-- | The consumer has indicated that there is some sort of problem
-- and continuation is impossible
data IterFailure = IterFailure String
  deriving (Typeable, Show)

instance Exception IterFailure where
  toException   = iExceptionToException
  fromException = iExceptionFromException

instance IException IterFailure

isIterFailure :: Exception e => e -> Bool
isIterFailure e = case fromException $ toException e of
    Just (IterFailure _) -> True
    _                    -> False


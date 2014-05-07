{-# LANGUAGE DeriveDataTypeable #-}

module Control.Timeout.Unique
    ( Unique(..)
    , newUnique
    ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)

newtype Unique = Unique Int64
    deriving (Eq, Ord, Typeable)

uniqSource :: IORef Int64
uniqSource = unsafePerformIO (newIORef 0)
{-# NOINLINE uniqSource #-}

newUnique :: IO Unique
newUnique = do
  r <- atomicModifyIORef' uniqSource $ \x -> let z = x + 1 in (z, z)
  return (Unique r)

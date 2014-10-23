{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Control.Timeout.TimerManager.Local
    ( registerTimeout
    , unregisterTimeout
    ) where

import Control.Applicative ((<*))
import Control.Exception (Exception, SomeException, handle, mask)
import Control.Concurrent (ThreadId, forkIO, throwTo, threadDelay, rtsSupportsBoundThreads)
import Control.Monad (void)
import Data.Typeable (Typeable)
import Data.Word (Word64)
import GHC.Conc (yield)
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)

import Data.IntPSQ (IntPSQ)

import qualified Data.IntPSQ as PSQ

import Control.Timeout.Types (Timeout(..))
import Control.Timeout.Unique (newUnique)

data Event = Register Int Int (IO ())
           | Unregister Int
    deriving (Typeable)

instance Show Event where
    show _ = "<<event>>"

instance Exception Event

type TimeoutQueue = IntPSQ Int (IO ())

tick :: (IO TimeoutQueue -> IO TimeoutQueue) -> TimeoutQueue -> IO TimeoutQueue
tick restore queue = handle eventHandler $ restore $ do
    let mbView = PSQ.minView queue
    case mbView of
        Just (_, prio, value, rest) -> do
            now <- getMonotonicTime
            let diff = prio - now
            threadDelay diff >> eatException value
            return $ rest
        Nothing -> threadDelay 1000000 >> return queue
  where
    eatException f = handle handleSomeException f
    handleSomeException :: SomeException -> IO ()
    handleSomeException _ = return ()
    eventHandler (Register timeout time f) = do
        now <- getMonotonicTime
        let time' = time + now
        return $ PSQ.insert timeout time' f queue
    eventHandler (Unregister timeout) = do
        return $ PSQ.delete timeout queue

loop :: IO ()
loop = void $ mask $ \restore -> go restore PSQ.empty
  where
    go restore state = tick restore state >>= go restore

managerThread :: ThreadId
managerThread
  | rtsSupportsBoundThreads = error "manager thread not running"
  | otherwise = unsafePerformIO $ forkIO loop <* yield
{-# NOINLINE managerThread #-}

registerTimeout :: Int -> IO () -> IO Timeout
registerTimeout time f = do
    timeout <- fmap Timeout newUnique
    throwTo managerThread $ Register (unsafeCoerce timeout) time f
    return timeout

unregisterTimeout :: Timeout -> IO ()
unregisterTimeout t = throwTo managerThread $ Unregister $ unsafeCoerce t

foreign import ccall unsafe "getMonotonicNSec"
    getMonotonicNSec :: IO Word64

getMonotonicTime :: IO Int
getMonotonicTime = fmap (fromIntegral . (`div` 1000)) $ getMonotonicNSec

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Timeout.EventManager.Local () where

import Control.Exception (Exception, handle)
import Control.Concurrent (ThreadId, forkIO, throwTo, rtsSupportsBoundThreads)
import Control.Monad (void)
import Data.Time.Clock (NominalDiffTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.State.Strict (evalStateT, get, put)

import Control.Timeout.EventManager.Types (Timeout)
import Control.Timeout.EventManager.PSQ (PSQ, Elem(..))
import Control.Timeout.Utils (sleep)
import qualified Control.Timeout.EventManager.PSQ as PSQ

data Event = Register Timeout NominalDiffTime (IO ())
           | Unregister Timeout
    deriving (Typeable)

instance Show Event where
    show _ = "<<event>>"

instance Exception Event

type TimeoutQueue = PSQ (IO ())
type EventManager = TimeoutQueue

tick :: EventManager -> IO EventManager
tick queue = handle eventHandler $ do
    let mbNext = PSQ.findMin queue
    case mbNext of
        Just (E { prio, value }) -> do
            now <- getCurrentTime
            let diff = diffUTCTime prio now
            sleep diff >> value
            return $ PSQ.deleteMin queue
        Nothing -> sleep 1 >> return queue
  where
    eventHandler (Register timeout time f) = do
        now <- getCurrentTime
        let time' = addUTCTime time now
        return $ PSQ.insert timeout time' f queue
    eventHandler (Unregister timeout) = return $ PSQ.delete timeout queue

loop :: IO ()
loop = void $ evalStateT go PSQ.empty
  where
    go = do
        state <- get
        new <- liftIO $ tick state
        put new >> go

managerThread :: ThreadId
managerThread
  | rtsSupportsBoundThreads = error "manager thread not running"
  | otherwise = unsafePerformIO $ forkIO loop
{-# NOINLINE managerThread #-}

registerTimeout :: NominalDiffTime -> IO () -> IO Timeout
registerTimeout = undefined

unregisterTimeout :: Timeout -> IO ()
unregisterTimeout t = throwTo managerThread $ Unregister t

module Control.Timeout.EventManager
    ( Timeout(..)
    , TimeoutKey
    , registerTimeout
    , unregisterTimeout
    ) where

import qualified GHC.Event as Event
import Control.Concurrent (rtsSupportsBoundThreads)

import Control.Timeout.Types (Timeout)

getTimerManager :: IO EventManager
getTimerManager
  | rtsSupportsBoundThreads = Event.getSystemTimerManager
  | otherwise = undefined

registerTimeout :: NominalDiffTime -> IO () -> IO Timeout
registerTimeout t f = do
    timer <- Event.getSystemTimerManager
    fmap Timeout $ registerTimeout timer (timeToUsecs t) f

unregisterTimeout :: Timeout -> IO ()
unregisterTimeout (Timeout key) = do
    timer <- Event.getSystemTimerManager
    Event.unregisterTimeout timer key

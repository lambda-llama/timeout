{-# LANGUAGE CPP #-}

module Control.Timeout.TimerManager
    ( registerTimeout
    , unregisterTimeout
    ) where

import Control.Concurrent (rtsSupportsBoundThreads)
import Data.Time.Clock (NominalDiffTime)
import Unsafe.Coerce (unsafeCoerce)
import qualified GHC.Event as Event

import Control.Timeout.Types (Timeout(..))
import Control.Timeout.Utils (timeToUsecs)
import qualified Control.Timeout.TimerManager.Local as Local

registerTimeout :: NominalDiffTime -> IO () -> IO Timeout
registerTimeout t f
  | rtsSupportsBoundThreads = do
        timer <- Event.getSystemTimerManager
        fmap (Timeout . unsafeCoerce) $ Event.registerTimeout timer (timeToUsecs t) f
  | otherwise = Local.registerTimeout t f

unregisterTimeout :: Timeout -> IO ()
unregisterTimeout t@(Timeout key)
  | rtsSupportsBoundThreads = do
#if __GLASGOW_HASKELL__ < 707
        Just timer <- Event.getSystemEventManager
#else
        timer <- Event.getSystemTimerManager
#endif
        Event.unregisterTimeout timer (unsafeCoerce key)
  | otherwise = Local.unregisterTimeout t

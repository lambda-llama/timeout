module Control.Timeout.EventManager
    ( registerTimeout
    , unregisterTimeout
    ) where

import qualified GHC.Event as Event
import Control.Concurrent (rtsSupportsBoundThreads)
import Data.Time.Clock (NominalDiffTime)
import Unsafe.Coerce (unsafeCoerce)

import Control.Timeout.Types (Timeout(..))
import Control.Timeout.Utils (timeToUsecs)
import qualified Control.Timeout.EventManager.Local as Local

registerTimeout :: NominalDiffTime -> IO () -> IO Timeout
registerTimeout t f
  | rtsSupportsBoundThreads = do
        timer <- Event.getSystemTimerManager
        fmap (Timeout . unsafeCoerce) $ Event.registerTimeout timer (timeToUsecs t) f
  | otherwise = Local.registerTimeout t f

unregisterTimeout :: Timeout -> IO ()
unregisterTimeout t@(Timeout key)
  | rtsSupportsBoundThreads = do
        timer <- Event.getSystemTimerManager
        Event.unregisterTimeout timer (unsafeCoerce key)
  | otherwise = Local.unregisterTimeout t

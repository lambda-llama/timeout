module Control.Timeout.Utils
    ( timeToUsecs
    , sleep
    ) where

import Control.Concurrent (threadDelay)
import Data.Time.Clock (NominalDiffTime)

import Control.Monad.Trans (MonadIO, liftIO)

timeToUsecs :: NominalDiffTime -> Int
timeToUsecs t = floor $ (* 1000000) $ toRational t

-- | Sleep for 'NominalDiffTime', example:
--
-- > sleep 5  -- Will sleep for 5 seconds
sleep :: (MonadIO m) => NominalDiffTime -> m ()
sleep = liftIO . threadDelay . timeToUsecs

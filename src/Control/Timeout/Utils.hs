module Control.Timeout.Utils
    ( timeToUsecs
    ) where

import Data.Time.Clock (NominalDiffTime)

timeToUsecs :: NominalDiffTime -> Int
timeToUsecs t = floor $ (* 1000000) $ toRational t

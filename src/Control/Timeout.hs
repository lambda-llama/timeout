-- |
-- Module     : Control.Timeout
-- Copyright  : 2014 Fedor Gogolev, 2013 Selectel
-- License    : MIT
-- Maintainer : Fedor Gogolev <knsd@knsd.net>
-- Stability  : unstable
--
-- This module provides simple interface for 'IO' time operations.
--
-- Example:
--
-- > module Main where
-- >
-- > import Control.Timeout (timeout, sleep)
--
-- > main :: IO ()
-- > main = do
-- >     timeout 1 $ sleep 2  -- Will return IO Nothing
-- >     timeout 2 $ sleep 1  -- Will return IO (Just ())
-- >     return ()
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Timeout
    ( NominalDiffTime
    , timeout
    , sleep
    ) where

import Control.Exception (Exception)
import Control.Concurrent (myThreadId, forkIO, killThread, threadDelay, throwTo)
import Data.Typeable (Typeable)
import Data.Time.Clock (NominalDiffTime)
import Data.Unique (Unique, newUnique)

import Control.Monad.Catch (MonadCatch(..), bracket, handleJust)
import Control.Monad.Trans (MonadIO, liftIO)

-- | Exception used for timeout handling
newtype Timeout = Timeout Unique
    deriving (Eq, Typeable)

instance Show Timeout where
    show _ = "<<timeout>>"

instance Exception Timeout

-- | Try to execute any 'MonadIO' 'a' action in timeout.
-- If timeout occured — return 'Nothing' else 'Just' a.
timeout :: (MonadCatch m, MonadIO m) => NominalDiffTime -> m a -> m (Maybe a)
timeout t f | t <= 0 = return Nothing
            | otherwise = do
    pid <- liftIO myThreadId
    ex  <- liftIO newUnique >>= return . Timeout
    handleJust (\e -> if e == ex then Just () else Nothing)
               (\_ -> return Nothing)
               (bracket (liftIO $ forkIO (sleep t >> throwTo pid ex))
                        (liftIO . killThread)
                        (\_ -> f >>= return . Just))

-- | Sleep for 'NominalDiffTime', example:
--
-- > sleep 5  -- Will sleep for 5 seconds
sleep :: (MonadIO m) => NominalDiffTime -> m ()
sleep = liftIO . threadDelay . floor . (* 1000000) . toRational

-- |
-- Module      : Control.Timeout
-- Copyright   : 2014 Fedor Gogolev, 2013 Selectel
-- License     : MIT
-- Maintainer  : Fedor Gogolev <knsd@knsd.net>
-- Stability   : unstable
-- Portability : non-portable
--
-- This module provides generalized 'sleep' and 'timeout' functions.
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

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RecursiveDo #-}

module Control.Timeout
    ( NominalDiffTime
    , Timeout(..)
    , withTimeout
    , timeout
    , sleep
    ) where

import Control.Concurrent (myThreadId, forkIO, killThread, throwTo,
                           rtsSupportsBoundThreads)
import Data.Time.Clock (NominalDiffTime)
import Data.Unique (newUnique)
import GHC.Event (getSystemTimerManager, registerTimeout, unregisterTimeout)
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad.Catch (MonadCatch(..), bracket, handleJust)
import Control.Monad.Trans (MonadIO, liftIO)

import Control.Timeout.EventManager.Types
import Control.Timeout.Utils

-- | Wrap an 'MonadIO' computation to time out and return @Nothing@ in case no result
-- is available within @n@ seconds. In case a result
-- is available before the timeout expires, @Just a@ is returned. A negative
-- timeout interval means \"timeout immediately\".
--
-- The design of this combinator was guided by the objective that @timeout n f@
-- should behave exactly the same as @f@ as long as @f@ doesn't time out. This
-- means that @f@ has the same 'myThreadId' it would have without the timeout
-- wrapper. Any exceptions @f@ might throw cancel the timeout and propagate
-- further up. It also possible for @f@ to receive exceptions thrown to it by
-- another thread.
--
-- A tricky implementation detail is the question of how to abort an @IO@
-- computation. This combinator relies on asynchronous exceptions internally.
-- The technique works very well for computations executing inside of the
-- Haskell runtime system, but it doesn't work at all for non-Haskell code.
-- Foreign function calls, for example, cannot be timed out with this
-- combinator simply because an arbitrary C function cannot receive
-- asynchronous exceptions. When @timeout@ is used to wrap an FFI call that
-- blocks, no timeout event can be delivered until the FFI call returns, which
-- pretty much negates the purpose of the combinator. In practice, however,
-- this limitation is less severe than it may sound. Standard I\/O functions
-- like 'System.IO.hGetBuf', 'System.IO.hPutBuf', Network.Socket.accept, or
-- 'System.IO.hWaitForInput' appear to be blocking, but they really don't
-- because the runtime system uses scheduling mechanisms like @select(2)@ to
-- perform asynchronous I\/O, so it is possible to interrupt standard socket
-- I\/O or file I\/O using this combinator.
timeout :: (MonadCatch m, MonadIO m) => NominalDiffTime -> m a -> m (Maybe a)
timeout = (. const) . withTimeout

withTimeout :: (MonadCatch m, MonadIO m) => NominalDiffTime -> (Timeout -> m a) -> m (Maybe a)
withTimeout t f | t <= 0 = return Nothing
            | rtsSupportsBoundThreads = do
    timer <- liftIO getSystemTimerManager
    ex@(Timeout key) <- liftIO $ mdo
        pid <- liftIO myThreadId
        ex <- fmap (Timeout . unsafeCoerce) (liftIO $ registerTimeout timer (timeToUsecs t) (throwTo pid ex))
        return ex
    handleJust (\e -> if e == ex then Just () else Nothing)
               (\_ -> return Nothing)
               (f ex >>= \r -> (liftIO $ unregisterTimeout timer (unsafeCoerce key)) >> (return $ Just r))
            | otherwise = do
    pid <- liftIO myThreadId
    ex  <- liftIO newUnique >>= return . Timeout . unsafeCoerce
    handleJust (\e -> if e == ex then Just () else Nothing)
               (\_ -> return Nothing)
               (bracket (liftIO $ forkIO (sleep t >> throwTo pid ex))
                        (liftIO . killThread)
                        (\_ -> f ex >>= return . Just))

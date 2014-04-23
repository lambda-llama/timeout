{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Timeout.Tests (tests) where

import Control.Exception (SomeException, try)
import Data.Maybe (isJust, isNothing)
import Data.Time.Clock (diffUTCTime, getCurrentTime)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary(..), Property, Positive(..), suchThat, once, within, testProperty)
import Test.QuickCheck.Monadic (monadicIO, run, assert)

import Control.Timeout (timeout, sleep)

-- | Interval that lesser than 1000 microseconds, that guarantied by
-- 'Arbitrary' instance implementation.
newtype SmallInterval = SmallInterval Int
    deriving (Show)

instance Arbitrary SmallInterval where
    arbitrary = fmap (SmallInterval . getPositive) $ suchThat arbitrary (< 1000)

-- | Timeout works with exceptions mechanism, so we need to check is
-- ordinary exceptions works.
testOtherException :: Property
testOtherException = monadicIO $ do
    res <- run $ try $ timeout 1 $ error "testOtherException"
    assert $ case res of
        Right _ -> False
        Left (_ :: SomeException) -> True

-- | Test is 'timeout' works.
testTimedOut :: Property
testTimedOut = monadicIO $ do
    res <- run $ timeout 0.1 $ sleep 0.2
    assert $ isNothing res

-- | Test is 'timeout' works even in negative case.
testNotTimedOut :: Property
testNotTimedOut = monadicIO $ do
    res <- run $ timeout 0.2 $ sleep 0.1
    assert $ isJust res

-- | Test is forked timeout thread killed properly.
testKillThreadKilled :: Property
testKillThreadKilled = monadicIO $ do
    run $ timeout 0.1 $ return ()
    run $ sleep 0.2
    assert True

-- | Test is 'sleep' actually sleep.
testSleep :: SmallInterval -> Property
testSleep (SmallInterval interval) = monadicIO $ do
    res <- run $ do
        now <- getCurrentTime
        sleep t
        new <- getCurrentTime
        return $ diffUTCTime new now
    assert $ res > t
  where
    t = fromIntegral interval / 1000

tests :: TestTree
tests = testGroup "Control.Timeout.Tests"
    [ testProperty "timeout pass exceptions" $ once $ testOtherException
    , testProperty "timed out" $ once testTimedOut
    , testProperty "not timed out" $ once testNotTimedOut
    , testProperty "kill thread killed" $ once testKillThreadKilled
    , testProperty "sleep" testSleep
    ]

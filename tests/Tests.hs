module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Control.Timeout.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ Control.Timeout.Tests.tests
    ]

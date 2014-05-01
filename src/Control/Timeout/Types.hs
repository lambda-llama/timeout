{-# LANGUAGE DeriveDataTypeable #-}

module Control.Timeout.Types
    ( Timeout(..)
    , TimeoutKey
    ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Unique (Unique)

type TimeoutKey = Unique

-- | Exception used for timeout handling
newtype Timeout = Timeout TimeoutKey
    deriving (Eq, Ord, Typeable)

instance Show Timeout where
    show _ = "<<timeout>>"

instance Exception Timeout

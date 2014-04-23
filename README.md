timeout [![Build Status][travis-img]][travis]
=======

[travis]: http://travis-ci.org/lambda-llama/timeout
[travis-img]: https://secure.travis-ci.org/lambda-llama/timeout.png

Example
-------

```haskell
module Main where

import Control.Timeout (timeout, sleep)

main :: IO ()
main = do
    timeout 1 $ sleep 2  -- Will return IO Nothing
    timeout 2 $ sleep 1  -- Will return IO (Just ())
    return ()
```

timeout [![Build Status][travis-img]][travis]  [![Build Status][coveralls-img]][coveralls]
=======

Generalized sleep and timeout functions.

[travis]: http://travis-ci.org/lambda-llama/timeout
[travis-img]: http://img.shields.io/travis/lambda-llama/timeout.svg?style=flat

[coveralls]: https://coveralls.io/r/lambda-llama/timeout
[coveralls-img]: http://img.shields.io/coveralls/lambda-llama/timeout.svg?style=flat

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

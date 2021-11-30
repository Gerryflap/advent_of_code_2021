module Preparation
    ( fib
    ) where

  fib :: Num a => [a]
  fib = 0:1:zipWith (+) fib (tail fib)

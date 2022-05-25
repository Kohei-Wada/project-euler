module Problems.Problem7 where

import Control.Monad.State






isPrime :: Integer -> Bool
isPrime n = factor n == [1, n]
    where factor n = [x | x <- [1..n], n `mod` x == 0]






problem7 = print $ isPrime 23

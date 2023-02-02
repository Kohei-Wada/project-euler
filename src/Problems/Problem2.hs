module Problems.Problem2 where

import Fibonacci (fib)


problem2 = do 
    print $ sum $ filter even $ takeWhile (< 4000000) [ fib n | n <- [0..]]

module Problems.Problem2 where

import Fibonacci

targets = filter (even) $ takeWhile (< 400000000000000) [ fib n | n <- [0..]]

problem2 = print $ sum targets





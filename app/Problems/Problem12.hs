module Problems.Problem12 where

import Factors



triangular n = n * (n - 1) `div` 2


triangulars = [triangular n | n <- [1..10000] ]


problem12 :: IO () 
problem12 = print $ triangulars

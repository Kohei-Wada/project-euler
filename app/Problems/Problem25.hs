module Problems.Problem25 where

import Fibonacci



numOrder :: Integer -> Int
numOrder = length . show 


problem25 :: IO () 
problem25 = do 
    print $ length $ takeWhile (\x -> numOrder x < 1000)  [ fib x | x <- [0..]]


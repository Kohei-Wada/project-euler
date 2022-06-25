module Problems.Problem34 where

import Factors



convert :: Integer -> Integer
convert 0 = 0
convert n = factorial (n `mod` 10) + convert (n `div` 10) 


order :: Integer -> Integer
order 0 = 0
order n = 1 + order (n `div` 10)


problem34 :: IO () 
problem34 = do 
    print $ sum $ filter (\x -> convert x == x) [3..1000000]

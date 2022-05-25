module Problems.Problem1 where

problem1 :: IO () 
problem1 = print $ sum [x | x <- [1..1000], (x `mod` 3 == 0) && (x `mod` 5 == 0)]

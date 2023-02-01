module Problems.Problem12 where

import Factors



triangular n = n * (n - 1) `div` 2

triangulars = [triangular n | n <- [1..] ]


problem12 :: IO () 
problem12 = do
    print $ length $ factors 76576500 
    print $ head $ dropWhile(\x -> (length . factors) x < 500) triangulars
    

module Factors where

import Control.Monad
import Data.List (foldl')


factors :: Integer -> [Integer]
factors 0 = []
factors n = go 1 [] where
    go acc xs 
      | acc == n       = xs ++ [acc]
      | mod n acc == 0 = go (acc + 1) (xs ++ [acc])
      | otherwise      = go (acc + 1) xs


factorization :: Integer -> [Integer]
factorization 1 = []
factorization n = v : factorization (n `div` v) 
    where v = factors n !! 1


primeFactors :: Integer -> [Integer]
primeFactors 0 = []
primeFactors n = go 2 n [] where
    go acc target xs 
      | target == 1 = xs
      | target `mod` acc == 0 = go 2 (target `div` acc) (xs ++ [acc])  
      | otherwise = go (acc + 1) target xs


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = go 1 n where
    go acc n 
      | n == 1 = acc
      | otherwise = go (acc * n) (n - 1) 


isPrime :: Integer -> Bool
isPrime n = if n <= 0 then False else factors n == [1, n]


sumPlaces :: Integer -> Integer
sumPlaces 0 = 0
sumPlaces n = n `mod` 10 + sumPlaces (n `div` 10)

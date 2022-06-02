module Factors where

import Control.Monad


factors :: Integer -> [Integer]
factors n = join $ do 
    let tmp  = filter (\x -> n `mod` x == 0) [1 .. (floor . sqrt . fromIntegral) n] 
    return $ tmp ++ map (n `div`) (reverse $ if last tmp ^ 2 == n then init tmp else tmp)


factorization :: Integer -> [Integer]
factorization 1 = []
factorization n = v : factorization (n `div` v) 
    where v = factors n !! 1


factorial :: Integer -> Integer 
factorial 0 = 1
factorial n = n * factorial (n - 1) 


isPrime :: Integer -> Bool
isPrime n = factors n == [1, n]


sumPlaces :: Integer -> Integer
sumPlaces 0 = 0
sumPlaces n = n `mod` 10 + sumPlaces (n `div` 10)


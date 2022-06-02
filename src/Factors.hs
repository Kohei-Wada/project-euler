module Factors where

import Control.Monad



factors :: Integer -> [Integer]
factors n = join $ do 
    let tmp  = filter (\x -> n `mod` x == 0) [1 .. (floor . sqrt . fromIntegral) n] 
        tmp' = if last tmp ^ 2 == n then drop 1 (reverse tmp) else reverse tmp 
    return $ tmp ++ map (n `div`) tmp'


factorization :: Integer -> [Integer]
factorization 1 = []
factorization n = v : factorization (n `div` v) 
    where v = factors n !! 1


factorial :: Integer -> Integer 
factorial 0 = 1
factorial n = n * factorial (n - 1) 


--Todo 
isPrime :: Integer -> Bool
isPrime n 
  | n == 2 || n == 3 || n == 5 || n == 7 = True
  | n `mod` 2 == 0 = False  
  | n `mod` 3 == 0 = False
  | n `mod` 5 == 0 = False
  | n `mod` 7 == 0 = False
  | otherwise = factorization n == [n] 



sumPlaces :: Integer -> Integer
sumPlaces 0 = 0
sumPlaces n = n `mod` 10 + sumPlaces (n `div` 10)

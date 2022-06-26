module Problems.Problem37 where

import Factors

truncateR :: Integer -> Integer
truncateR n = n `div` 10


truncateL :: Integer -> Integer 
truncateL n = (read . tail . show) n


truncatableR :: Integer -> Bool
truncatableR n 
  | n < 10 = isPrime n 
  | otherwise = let tmp = truncateR n in if isPrime tmp then truncatableR tmp else False
                     

truncatableL :: Integer -> Bool
truncatableL n 
  | n < 10 = isPrime n 
  | otherwise = let tmp = truncateL n in if isPrime tmp then truncatableL tmp else False


truncatablePrime :: Integer -> Bool
truncatablePrime n 
  | n < 10    = False
  | otherwise = isPrime n && truncatableL n && truncatableR n 


problem37 :: IO () 
problem37 = do 
      print $ sum $ filter truncatablePrime [0..1000000]

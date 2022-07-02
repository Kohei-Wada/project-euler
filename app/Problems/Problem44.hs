module Problems.Problem44 where

import Factors
import Data.Function
import Data.List


type Pentagonal = Int


pentagonal :: Int -> Int
pentagonal n = n * (3 * n - 1) `div` 2


isPentagonal :: Int -> Bool
isPentagonal n = let x = (1 + sqrt (1 + 24 * fromIntegral n)) / 6 :: Float
                  in x == realToFrac (truncate x)

diffOfPs :: (Int, Int) -> Int
diffOfPs (i, j) = abs $ pentagonal j - pentagonal i 


sumOfPs :: (Int, Int) -> Int
sumOfPs (i, j) = pentagonal j + pentagonal i 


check :: (Pentagonal, Pentagonal) -> Bool
check (pi, pj) = isPentagonal (pj - pi) && isPentagonal (pi + pi)


problem44 :: IO () 
problem44 = do 
    let ls = [(i, j) | i <- [1..], j <- [i..]]
    print 0

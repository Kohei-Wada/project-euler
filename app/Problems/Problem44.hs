module Problems.Problem44 where

import Factors
import Data.Function
import Data.List
import Polygonal



diffOfPs :: (Int, Int) -> Int
diffOfPs (i, j) = abs $ pentagonal j - pentagonal i 


sumOfPs :: (Int, Int) -> Int
sumOfPs (i, j) = pentagonal j + pentagonal i 


check :: (Polygonal, Polygonal) -> Bool
check (pi, pj) = isPentagonal (pj - pi) && isPentagonal (pi + pi)


problem44 :: IO () 
problem44 = do 
    let ls = [(i, j) | i <- [1..], j <- [i..]]
    print 0

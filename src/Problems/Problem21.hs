{-# LANGUAGE BangPatterns #-}
module Problems.Problem21 where

import Control.Monad (guard, forM_) 
import Data.List (group) 

d :: Int -> Int
d 0 = 0 
d n = go 1 0 where
    go di acc
      | di == n       = acc
      | mod n di == 0 = go (di + 1) (acc + di) 
      | otherwise     = go (di + 1) acc


d' :: Int -> Int
d' 0 = 0 
d' n = go 1 n 2 2 0 - n 
    where
        go !acc !target !di !l !count 
          | target == 1          = acc * geometoricSum di count
          | target `mod` di == 0 = if l /= di then go (acc * geometoricSum l count) (target `div` di) di di 1 
                                              else go acc (target `div` di) di di (count + 1) 
          |  otherwise           = go acc target (di + 1) l count

        geometoricSum _ 0 = 1 
        geometoricSum a n = a ^ n + geometoricSum a (n - 1) 


amicables :: [Int]
amicables = do 
    n <- [1..]
    let !tmp = d' n 
    guard $ tmp /= n 
    guard $ d' tmp == n 
    pure n 


problem21 :: IO () 
problem21 = do 
    let ans = sum $ takeWhile (< 10000) amicables
    print ans




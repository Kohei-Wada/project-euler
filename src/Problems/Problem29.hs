module Problems.Problem29 where

import Data.List.Unique


problem29 :: IO () 
problem29 = do 
    print $ length $ unique [a**b | a <- [2..100], b <- [2..100]]


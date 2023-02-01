module Problems.Problem52 where

import Data.List


check :: Int -> Int -> Bool 
check a b = (sort . show) a == (sort . show) b 


tmp :: Int -> Bool
tmp x = check x (2*x) && 
        check x (3*x) && 
        check x (4*x) && 
        check x (5*x) && 
        check x (6*x) 


problem52 :: IO () 
problem52 = do 
    let ans = head $ filter tmp [1..]
    print ans

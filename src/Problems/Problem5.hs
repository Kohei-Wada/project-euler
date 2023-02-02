module Problems.Problem5 where

import Data.List (foldl')

problem5 :: IO () 
problem5 = do  
    let ans = foldl' (lcm) 1 [1 .. 20]
    print ans

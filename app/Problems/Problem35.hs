module Problems.Problem35 where

import Eratosthenes
import Factors 


cycleNum :: Int -> Int 
cycleNum n = let (x:xs) = show n in read (xs ++ [x]) :: Int




problem35 :: IO () 
problem35 = do 
    print $ cycleNum 197

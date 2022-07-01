module Problems.Problem36 where

import Numeric

isBatch :: Integer -> Bool
isBatch n = let s = show n in s == reverse s 


isBatchBin :: Integer -> Bool 
isBatchBin n = let s = showBin n in s == reverse s


showBin :: Integer -> String
showBin 0 = "" 
showBin n = let b = n `mod` 2 in showBin ((n - b) `div`2) ++ show b


problem36 :: IO () 
problem36 = do 
    let l = [1..1000000]
    print $ sum $ filter (\n -> isBatch n && isBatchBin n) l 
 

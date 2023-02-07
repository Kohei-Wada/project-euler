module Problems.Problem12 where

import Data.List (group)
import Factors (primeFactors) 


triangulars :: [Int]
triangulars = [n * (n - 1) `div` 2 | n <- [2..] ]

countFactors :: Int -> Int
countFactors n = product $ map (\xs -> length xs + 1) $ group $ primeFactors $ fromIntegral n

problem12 :: IO () 
problem12 = do
    let ans = head $ dropWhile (\x -> countFactors x < 500 ) triangulars
    print ans


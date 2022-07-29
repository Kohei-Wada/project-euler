module Problems.Problem66 where

import ContinuedFraction

import Data.List
import Data.Function
import Data.Ratio


-- solution of x^2 - d* y^2 = 1
solution :: Int -> (Integer, Integer)
solution d = 
    let 
        cf = chainOfCFSqrt d 
        fr = toRatio $ init $ if even (length $ init cf) then cf else cf ++ tail cf
     in 
        (numerator fr, denominator fr)


problem66 :: IO () 
problem66 = do 
    let ans = maximumBy (compare `on` (fst . solution)) [1..1000]
    print ans


module ContinuedFraction where

import GHC.Real
import Data.Ratio


type ConFrac = [Int]


--Continued Fraction of SQRT finite
cfoSqrtf :: Int -> ConFrac 
cfoSqrtf d = 
    let 
        next :: (Int, Int, Int) -> (Int, Int, Int)
        next (p, q, a) = 
            let p':%q' = p % (d - q^2) 
                a'     = floor $ fromIntegral p / ((sqrt . fromIntegral) d - fromIntegral q)
             in (q', a' * q' - q, a')

        loop :: (Int, Int, Int) -> [Int] -> [(Int, Int)] -> [Int]
        loop t as fs = 
            let t'@(p', q', a) = next t 
             in if (p', q') `elem` fs then (as ++ [a]) else loop t' (as ++ [a]) (fs ++ [(p', q')])
    in 
        let tmp = (floor . sqrt . fromIntegral) d 
         in if tmp^2 == d then [tmp] else loop (1, tmp, tmp) [tmp] [(1, tmp)]


chainOfCFSqrt :: Int -> ConFrac 
chainOfCFSqrt = cfoSqrtf


cfoSqrt :: Int -> ConFrac 
cfoSqrt d = let tmp = cfoSqrtf d in if length tmp < 2 then tmp else head tmp:(cycle. tail) tmp


cfoE :: ConFrac
cfoE = 2:[if a `mod` 3 == 2 then 2 * (a `div` 3 + 1) else 1 | a <- [1..]] 


toRatio :: ConFrac -> Rational
toRatio []     = 0 -- Error 
toRatio (a:as) = fromIntegral a + if null as then 0 else 1 / toRatio as

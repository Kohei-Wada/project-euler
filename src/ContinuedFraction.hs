module ContinuedFraction where


import GHC.Real
import Data.Ratio

--Continued Fraction of SQRT
cfoSqrt :: Int -> [Int]
cfoSqrt d = 
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



--approximate fraction of cf
afoe :: Integer -> Rational
afoe 0 = 2
afoe n = 
    let 
        e = [if a `mod` 3 == 2 then 2 * (a `div` 3 + 1) else 1 | a <- [1..]] :: [Integer]

        loop :: Integer -> Rational
        loop 0 = (e!!fromIntegral (n - 1)) % 1
        loop x = (e!!fromIntegral (n - 1 - x)) % 1 + 1 / loop (x - 1) 

     in 2 + (1 / loop (n - 1))




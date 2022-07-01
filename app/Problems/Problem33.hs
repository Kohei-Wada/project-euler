module Problems.Problem33 where

import GHC.Real
import Data.Ratio



-- Is it a digit-cancelling fraction?
isDcf :: (Int, Int) -> Bool
isDcf (n, d) = let (a,  b ) = (n `div` 10, n `mod` 10) 
                   (a', b') = (d `div` 10, d `mod` 10) 
                in (a == b' && b*d == n*a') || (b == a' && a*d == n*b')


problem33 :: IO () 
problem33  = do 
    let qs = [(numer, denom) | denom <- [10..99], numer <- [10..denom-1]]
        candidates = filter isDcf qs
        ans = denominator $ product $ map (\(n,d) -> (n % d)) candidates

    print candidates

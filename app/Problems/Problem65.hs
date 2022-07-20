module Problems.Problem65 where

import Data.Ratio

--approximate fraction of e
afoe :: Integer -> Ratio Integer
afoe 0 = 2
afoe n = 
    let 
        e = [if a `mod` 3 == 2 then 2 * (a `div` 3 + 1) else 1 | a <- [1..]] :: [Integer]

        loop :: Integer -> Ratio Integer 
        loop 0 = e!!(fromIntegral (n - 1)) % 1
        loop x = e!!(fromIntegral (n - 1 - x)) %1  + 1 / loop (x - 1) 

     in 2 + (1 / loop (n - 1))


sumDigits :: Integral a => a -> a 
sumDigits 0 = 0 
sumDigits n = n `mod` 10 + sumDigits (n `div` 10)


problem65 :: IO () 
problem65 = do 
    let d   = denominator $ afoe 100
        ans =  sumDigits d

    print ans

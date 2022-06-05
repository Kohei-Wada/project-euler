module Problems.Problem21 where

import Factors


d :: Integer -> Integer
d = sum . init . factors


amicable :: Integer -> Integer 
amicable n = let tmp = d n in if (d tmp) == n then tmp else 0


problem21 :: IO () 
problem21 = do 
    print $ filter (\(x, a) -> a /= 0 && x /= a) $ map (\x -> (x, amicable x)) [220..1000000]

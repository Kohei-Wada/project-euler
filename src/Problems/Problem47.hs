module Problems.Problem47 where


import Data.Numbers.Primes 
import Data.List.Unique


splitOnSameValue :: Eq a => [a] -> [[a]]
splitOnSameValue [] = []
splitOnSameValue xs = let tmp = takeWhile (== head xs) xs 
                       in [tmp] ++ splitOnSameValue (drop (length tmp) xs)


problem47 :: IO () 
problem47 = do 
    let l   = splitOnSameValue $ map (length.uniq.primeFactors) [1..]
        ans = 1 + (sum $ map length $ takeWhile (/= [4, 4, 4, 4]) l)

    print ans


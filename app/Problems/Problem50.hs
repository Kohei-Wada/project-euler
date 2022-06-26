module Problems.Problem50 where

import Eratosthenes
import Factors
import Control.Monad


primes :: Integer -> [Integer]
primes n = map (fromIntegral) $ erat [2.. fromIntegral n]


convertList :: Int -> [Integer] -> [[Integer]]
convertList n as = if length as < n || 2 * head as * (fromIntegral n) > last as 
                      then [] 
                      else [take n as] ++ convertList n (tail as) 


safeLast :: [Integer] -> Integer 
safeLast [] = 0 
safeLast ns = last ns 


problem50 :: IO () 
problem50 = do 
    let n  = 1000000
        ps = primes n 

    let anss = join $ forM (reverse [21..1414]) $ \x -> do 
                let l   = convertList (fromIntegral x) ps
                    ans = safeLast $ filter (\tmp -> tmp < n && isPrime tmp) $ map (sum) l 
                return (x, ans) 
         
        answer = head $ filter (\x -> snd x /= 0) anss

    
    print answer


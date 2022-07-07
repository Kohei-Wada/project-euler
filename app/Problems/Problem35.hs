module Problems.Problem35 where

import Eratosthenes
import Factors 


rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]


cycleNum :: Int -> Int 
cycleNum n = read $ rotate $ show n 


cycleList :: Int -> [Int]
cycleList n = tmp (n, (length . show) n) 
    where tmp (n, l) = 
            if l /= 0 
               then let next = cycleNum n in [next] ++ tmp (next, l - 1) 
               else []


isCyclePrime :: Int -> Bool
isCyclePrime n = 
    if elem '0' (show n) 
       then False
       else foldl (\b n -> b && (isPrime . fromIntegral) n) True (cycleList n)


problem35 :: IO () 
problem35 = do 
    print $ filter isCyclePrime $ erat 1000000

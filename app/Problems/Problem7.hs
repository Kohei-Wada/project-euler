module Problems.Problem7 where

import Factor (isPrime, factorization) 



problem7 = do 
    print $ take 1000 $ filter isPrime [1, 3..1000000]

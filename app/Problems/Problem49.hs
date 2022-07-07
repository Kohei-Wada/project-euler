module Problems.Problem49 where

import Eratosthenes


problem49 :: IO () 
problem49 = do 
    let ps = erat 100
    print ps

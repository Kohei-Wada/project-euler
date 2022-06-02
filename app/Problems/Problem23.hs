{-# LANGUAGE RecordWildCards #-}

module Problems.Problem23 where

import Factors 


isPerfectNum n = 2 * n == (sum $ factors n)


problem23 :: IO ()
problem23 = do 
    print $ filter isPerfectNum [1..10000]


{-# LANGUAGE RecordWildCards #-}

module Problems.Problem23 where

import Data.List
import Data.List.Unique
import Factors 


isPerfectNum :: Integer -> Bool
isPerfectNum n = 2 * n == (sum $ factors n)


isAbundantNum :: Integer -> Bool
isAbundantNum n = 2 * n < (sum $ factors n)


abundantNums :: Integer -> [Integer]
abundantNums n = filter isAbundantNum [1..n]


--todo
problem23 :: IO ()
problem23 = do 
    let abndnts = filter isAbundantNum [12..28123]
        abnabn  = uniq $ sort [x + y | x <- abndnts, y <- abndnts, x + y < 28123]
        target  = filter (\x -> not $ elem x abnabn) [1..28123] 

    print $ sum target




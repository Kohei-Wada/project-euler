module Problems.Problem92 where


import Control.Monad
import Data.List

squareChain :: Int -> [Int]
squareChain n = if n == 1 || n == 89 then [n] else  let n' = next n in n:squareChain n'
    where 
        next 0 = 0
        next n = (n `mod` 10) ^2 + next (n `div` 10) 


problem92 :: IO () 
problem92 = do 

    let targets = [1..10000000]
        tmp = map squareChain targets
        ans = length $ filter ((== 89). last) tmp
    
    print ans

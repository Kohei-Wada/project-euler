module Problems.Problem95 where

import Control.Monad
import Data.Function

factors :: Int -> [Int]
factors 0 = []
factors n = join $ do 
    let tmp  = filter (\x -> n `mod` x == 0) [1 .. (floor . sqrt . fromIntegral) n] 
    return $ tmp ++ map (n `div`) (reverse $ if last tmp ^ 2 == n then init tmp else tmp)


divisor :: Int -> Int
divisor = sum .init. factors 


amicableChainLength :: Int -> Int
amicableChainLength n = 
    let 
        loop :: Int -> Int -> Int
        loop c x = 
            let tmp = divisor x 
             in 
                if tmp == 0 || tmp == x || tmp > 1000000 
                   then 0
                   else 
                    if tmp == n then c
                                else loop (c+1) tmp
    in loop 1 n


loop n= do 
    let tmp = divisor n
    print tmp 
    loop tmp


problem95 :: IO () 
problem95 = do 
    print $ map  amicableChainLength [1..1000]

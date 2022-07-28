module Problems.Problem64 where

import Data.Ratio
import GHC.Real



cf :: Int -> [Int]
cf d = 
    let 
        next :: (Int, Int, Int) -> (Int, Int, Int)
        next (p, q, a) = 
            let p':%q' = p % (d - q^2) 
                a'     = floor $ fromIntegral p / ((sqrt . fromIntegral) d - fromIntegral q)
             in (q', a' * q' - q, a')

        loop :: (Int, Int, Int) -> [Int] -> [(Int, Int)] -> [Int]
        loop t as fs = 
            let t'@(p', q', a) = next t 
             in if (p', q') `elem` fs then (as ++ [a]) else loop t' (as ++ [a]) (fs ++ [(p', q')])
    in 
        let tmp = (floor . sqrt . fromIntegral) d 
         in if tmp^2 == d then [tmp] else loop (1, tmp, tmp) [tmp] [(1, tmp)]
        


problem64 :: IO () 
problem64 = do
    let ans = length $ filter odd $ map (length . init . cf) [2..10000]
    print ans

module Problems.Problem75 where

import Control.Monad
import System.Exit
import Data.List
import Data.List.Unique
import Control.Parallel.Strategies 
import Control.DeepSeq
import Data.List.Split


combinations :: Integer -> [[Integer]]
combinations n = 
    let
        loop :: Integer -> [Integer] -> [[Integer]]
        loop _ [] = []
        loop n (k:ks) =
            let combk = if n `mod` k == 0 then combinationsK k (n `div` k) else []
             in combk ++ loop n ks
     in     
        filter (/= []) $ loop n [1..n]


combinationsK :: Integer -> Integer -> [[Integer]]
combinationsK k l  =  
    let
        loop _ [] = []
        loop l (m:ms) = 
            let n   = l`div`m - m 
                comb = if l `mod` m == 0 && n < m && n > 0 then [k, m, n] else []
             in [comb] ++ loop l ms
     in 
        loop l [2.. (floor.sqrt.fromIntegral) l] 


toPTs :: [Integer] -> [Integer]
toPTs ns = 
    let k = ns!!0 
        m = ns!!1 
        n = ns!!2  
     in sort $ map (*k) [m^2 - n^2, 2*m*n, m^2 + n^2]


sameLengthKMN :: Integer -> [[Integer]]
sameLengthKMN l = combinations (l `div` 2) 


sameLengthPts :: Integer -> [[Integer]]
sameLengthPts l = uniq $ sort $  map toPTs $ sameLengthKMN l 


--TODO
problem75 :: IO () 
problem75 = do 
    let ls = [12, 14 .. 150000]
        cs = chunksOf (length ls `div` 20) ls
        soltions = runEval $ do 
            cs' <- forM cs (\c -> rpar $ force $ filter (\ps -> length ps == 1) (map sameLengthPts c))
            forM_ cs' rseq
            return $ concat cs'

    print $ length soltions


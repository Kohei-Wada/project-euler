module Problems.Problem32 where

import Data.List
import Data.List.Split

import Control.Monad
import Data.Function


type SplitWay = [[Int]]


list2num :: [Int] -> Int
list2num []     = 0
list2num (n:ns) = 10 ^ length ns * n + list2num ns


splitWays :: [a] -> SplitWay
splitWays as = let l  = length as 
                   ns = [1..length as] 
                in [[x, y, z] | x <- ns, y <- ns, z <- ns, x + y + z == l]


isPDPair :: [[Int]] -> Bool
isPDPair nss = 
    let ns = map list2num nss in (ns!!0) * (ns!!1) == (ns!!2) 


reduce :: [[Int]] -> [[Int]]
reduce nss = let nss' = sortBy (compare `on` last) nss in tmp nss'
             where
                 tmp [] = []
                 tmp (ns:nss) = [ns] ++ 
                     if (last . head) nss == last ns then reduce $ tail nss else reduce nss


problem32 :: IO () 
problem32 = do 
    let ns   = [1..9]
        pds  = permutations ns
        ws   = splitWays ns
        ans  = filter isPDPair [ splitPlacesBlanks w pd | w <- ws, pd <- pds ]

    print $ sum $ map product $ reduce $ map (map list2num) ans

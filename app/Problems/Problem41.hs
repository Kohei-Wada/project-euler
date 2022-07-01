module Problems.Problem41 where

import Data.List
import Factors

order :: Int -> Int
order = length . show


isPandigital :: Int -> Bool 
isPandigital n = (sort . show) n == concat [show x | x <- [1.. order n]]


list2num :: [Int] -> Int
list2num xs = read $ concat (map show xs)


pdsN :: Int -> [Integer] 
pdsN n = map (fromIntegral. list2num) $ permutations [1..n]


problem41 :: IO () 
problem41 = do 
    let pds = concat [pdsN n | n <- [1..9]]
        ans = maximum $ filter isPrime pds

    print ans

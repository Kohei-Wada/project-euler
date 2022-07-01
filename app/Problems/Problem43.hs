module Problems.Problem43 where

import Data.List


list2num :: [Int] -> Int
list2num xs = read $ concat (map show xs)


check :: [Int] -> Bool
check ns = 
    list2num [ns!!1, ns!!2, ns!!3]  `mod` 2  == 0 &&
    list2num [ns!!2, ns!!3, ns!!4]  `mod` 3  == 0 &&
    list2num [ns!!3, ns!!4, ns!!5]  `mod` 5  == 0 &&
    list2num [ns!!4, ns!!5, ns!!6]  `mod` 7  == 0 &&
    list2num [ns!!5, ns!!6, ns!!7]  `mod` 11 == 0 &&
    list2num [ns!!6, ns!!7, ns!!8]  `mod` 13 == 0 &&
    list2num [ns!!7, ns!!8, ns!!9]  `mod` 17 == 0 


problem43 :: IO () 
problem43 = do 
    let pds = filter (\ns -> head ns /= 0) $ permutations [0..9]
        ans = sum $ map list2num $ filter check pds
    
    print ans


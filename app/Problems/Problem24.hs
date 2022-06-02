module Problems.Problem24 where

import Data.List

list2String :: [Int] -> String
list2String [] = []
list2String (n:l)  = show n ++ list2String l



problem24 :: IO () 
problem24 = do 
    let l = sort $ map list2String $ permutations [0..9]
    print $ l !! 1000000

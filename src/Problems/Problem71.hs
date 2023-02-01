{-# LANGUAGE BangPatterns #-}
module Problems.Problem71 where

import Control.Monad (guard, forM_) 

{- Farey Sequence 
     https://en.wikipedia.org/wiki/Farey_sequence
-}

farey :: Int -> [(Int, Int)]
farey n = go 1 [(0, 1), (1, 1)] where
    go k xs = 
        let 
        update [x] = [x]
        update (x0@(n0, d0) : x1@(n1, d1) : xs) = 
            if d0 + d1 == k + 1 then x0 : (n0 + n1, d0 + d1) : update (x1 : xs) 
                                else x0 :                      update (x1 : xs)
         in 
         if k == n then xs
                   else go (k + 1) (update xs)


findAnswer :: Int -> (Int, Int) -> (Int, Int)
findAnswer n x
  | n < 8 = undefined
  | otherwise = go 8 (2, 5) x where
        go k x0@(n0, d0) x1@(n1, d1) = 
            if d0 + d1 > n then (n0, d0) else go (d0 + d1) (n0 + n1, d0 + d1) x1


problem71 :: IO () 
problem71 = do 

    {- TEST for findAnswer
    forM_ [8 .. 50] $ \n -> do 
        let t1 =  findAnswer n (3, 7)
            t2 = last $ takeWhile (/= (3, 7)) $ farey n
        print $ t1 == t2
    -}
    
    let ans = findAnswer 1000000 (3, 7) 
    print ans

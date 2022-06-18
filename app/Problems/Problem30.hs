module Problems.Problem30 where


import Data.Array
import Data.List
 
-- 重複組み合わせ
repComb :: [a] -> Int -> [[a]]
repComb xs n = loop (func ([], xs)) n
    where
      loop xss 1 = [xs | (xs, _) <- xss]
      loop xss n = loop (concatMap func xss) (n - 1)
      func (xs, ys) = [(xs ++ [z], zs) | zs@(z : _) <- tails ys]
 


integralToList :: Integral a => a -> a -> [a]
integralToList radix n = iter n []
    where
      iter n prd
          | n < radix = n : prd
          | otherwise = iter (div n radix) (rem n radix : prd)
 

problem30 :: IO () 
problem30 = print $ sum $ concatMap ns [2 .. 6]
    where
      ns m = [n | xs <- repComb [0 .. 9] m, let n = calc xs, check xs n]
      n5 = listArray (0, 9) [x ^ 5 | x <- [0 .. 9]]
      calc ys = sum $ map (n5 !) ys
      check ys n = (sort $ integralToList 10 n) == ys
 

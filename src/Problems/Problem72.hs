{-# LANGUAGE ExplicitForAll #-}
module Problems.Problem72 where
    
import Data.List (foldl')
import Control.Monad (forM_) 


{- Farey Sequence 
     https://en.wikipedia.org/wiki/Farey_sequence
-}


uniq :: forall a. Ord a => [a] -> [a]
uniq [] = []
uniq (ini:as) = foldl' (\list@(x:xs) a -> if x == a then list else a:list) [ini] as

primeFactors :: Int -> [Int]
primeFactors n = go 2 n [] where
    go _ 1 xs = xs 
    go acc target xs = 
        if target `mod` acc == 0 then go 2 (target `div` acc) (acc:xs)
                                 else go (acc + 1) target xs

eulersTotient :: Int -> Int
eulersTotient n = go ((uniq . primeFactors) n) n where
    go [] x = x
    go (a:as) x = go as (x * (a - 1) `div` a) 


fareyLength :: Int -> Int
fareyLength n = foldl' (\acc m -> acc + eulersTotient m) 1 [1..n]


problem72 :: IO () 
problem72 = do 
    forM_ [1 .. 100000] $ \n -> do 
        print $ eulersTotient n 

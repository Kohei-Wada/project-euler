module Problems.Problem49 where

import Number 
import Data.Numbers.Primes 
import Data.List
import Data.Maybe


type Relation a = a -> a -> Bool

quotBy :: Integral a => Relation a -> [a] -> [[a]]
quotBy _ [] = []
quotBy r xs = 
    let x         = head xs 
        (as, as') = split (relation x) xs 
     in [as] ++ quotBy r as'
        where split f xs = let eqc = filter f xs in (eqc, deleteElems eqc xs)


deleteElems :: Eq a => [a] -> [a] -> [a]
deleteElems [] xs = xs
deleteElems (e:es) xs = deleteElems es (delete e xs)


relation :: Integral a => Relation a
relation x y = sort (splitByDigit x) == sort (splitByDigit y)


splitByDigit :: Integral a => a -> [a]
splitByDigit 0 = []
splitByDigit n = [n `mod` 10] ++ splitByDigit (n `div` 10)


tmp xs = map (\x -> x - head xs) xs


isArithmetic :: (Num a, Eq a) => (a, a, a) -> Bool 
isArithmetic (a, b, c) = b - a == c - b


hasArithmetic :: [Int] -> Maybe (Int, Int, Int)
hasArithmetic xs =  
    let tmp = filter isArithmetic [(a, b, c) | a <- xs , b <- xs, c <- xs , a < b, b < c]
     in if null tmp then Nothing else Just (head tmp)



problem49 :: IO () 
problem49 = do 
    let ps  = filter (\n -> order n == 4) $ takeWhile (\n -> order n <= 4) primes
        pss = filter (\ps -> length ps >= 3) (quotBy relation ps)
        Just (a, b, c) =  last $ filter (not . null) $ map hasArithmetic pss
        ans = show a ++ show b ++ show c
     
    putStrLn ans

    
    

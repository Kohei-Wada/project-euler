module Problems.Problem27 where

import Factors
import Eratosthenes

import Data.List
import Data.Function


eulerPolynominal :: (Integer, Integer) -> (Integer -> Integer)
eulerPolynominal (a, b) = \x -> x*x + a*x + b 


primesLength :: (Integer -> Integer) -> Integer
primesLength p = tmp p 0
    where tmp p n = if isPrime (p n) then 1 + tmp p (n + 1) else 0 


problem27 :: IO () 
problem27 = do 
    let ps = erat 1000

    let targets = [(a, fromIntegral b) | a <- [-1000..1000], b <- ps ++ map (*(-1)) ps]
        lenList = map (\x -> (x, (primesLength . eulerPolynominal) x)) targets

    print $ let t@(a, b) = fst $ maximumBy (compare `on` snd) lenList
             in a * b



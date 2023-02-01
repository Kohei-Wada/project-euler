module Problems.Problem42 where

import System.IO 
import Data.Char
import Data.List.Split


file = "data/problem42.txt"

triangle n = n * (n+1) `div` 2

isTriangleNum :: Int -> Bool
isTriangleNum n = elem n $ takeWhile (\x -> x <= n) [ triangle x | x <- [0..] ]


wordScore :: String -> Int
wordScore [] = 0
wordScore (c:cs) = ord c + wordScore cs
    where score c = ord c - 64


problem42 :: IO () 
problem42 = do 
    s <- readFile file 
    let list = map (init . tail) $ splitOn "," s
        scores = map wordScore list

    print $ length $ filter isTriangleNum scores  

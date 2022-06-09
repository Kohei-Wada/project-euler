module Problems.Problem18 where

import Control.Applicative

list = [75, 95,64, 17,47,82, 18,35,87,10, 20,04,82,47,65, 19,01,23,75,03,34, 88,02,77,73,07,63,67,99,65,04,28,06,16,70,92,41,41,26,56,83,40,80,70,33,41,48,72,33,47,32,37,16,94,29,53,71,44,65,25,43,91,52,97,51,14, 70,11,33,28,77,73,17,78,39,68,17,57, 91,71,52,38,17,14,91,43,58,50,27,29,48, 63,66,04,68,89,53,67,30,73,16,69,87,40,31, 04,62,98,27,23,09,70,98,73,93,38,53,60,04,23] :: [Int]


list2Triangle :: [a] -> [[a]]
list2Triangle = makeTriangle 1
    where 
        makeTriangle _ [] = []
        makeTriangle n xs = [take n xs] ++ makeTriangle (n + 1) (drop n xs) 


largersOfEach2 :: Ord a => [a] -> [a]
largersOfEach2 xs = 
    if length xs /= 1 
       then [maximum (take 2 xs)] ++ largersOfEach2 (drop 1 xs)
       else []


shrink :: (Num a, Ord a) => [[a]] -> [[a]]
shrink (xs0:xs1:xs) = [sumLists (largersOfEach2 xs0) xs1] ++ xs
    where 
        sumLists :: Num a => [a] -> [a] -> [a]
        sumLists [] _ = []
        sumLists _ [] = []
        sumLists (x:xs) (y:ys) = [x + y] ++ sumLists xs ys


shrink1 :: (Num a, Ord a) => [[a]] -> [[a]] 
shrink1 xs = if length xs == 1 then xs else shrink1 $ shrink xs


problem18 :: IO () 
problem18 = do 
    let graph = reverse $ list2Triangle list 
    print $ shrink1 graph


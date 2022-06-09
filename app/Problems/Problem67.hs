module Problems.Problem67 where

import Data.List.Split
import Control.Monad
import Problems.Problem18



triangle = "data/triangle.txt"

problem67 :: IO () 
problem67 = do 
    str <- readFile triangle 
    let tmp =  map (splitOn ",") $ lines str 
        graph =  reverse $ map (\xs -> map (\x -> read x :: Int) xs) tmp 

    print $ shrink1 graph





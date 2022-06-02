module Main where
    
import Problems.Problem23
import Factors 


main :: IO ()
main = do 
    print $ filter isPrime [1..100]

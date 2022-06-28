module Main where
    
import Quotient
import Control.Monad


main :: IO ()
main = do 
    let ls = [ quotient (1, x) | x <- [1..10000]]
    print $ q2d $ quotient (1, 998)

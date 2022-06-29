module Main where
    
import Problems.Problem32
import Quotient
import Control.Monad


main :: IO ()
main = do 
    let target = [ quotient (1, x) | x <- [1, 3, 11, 27, 101, 41, 7, 239, 73, 81, 451, 21649, 707]]
    forM_ target (print . q2d)

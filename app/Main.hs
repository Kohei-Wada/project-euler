module Main where
    
import Problems.Problem26

import Data.Ratio
import Decimal
import Control.Monad


main :: IO ()
main = do 
    let l = [ 1 % x | x <- [1..1000]]
    
    print $ toDecimal $ 1 % 7
    print $ toDecimal $ 1 % 14
    print $ toDecimal $ 1 % 21
    print $ 714285 + 333333



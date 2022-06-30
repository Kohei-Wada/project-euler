module Problems.Problem33 where

import Data.Ratio


problem33 :: IO () 
problem33 = do 
    let ps = [(x ,y) | x <- [1..99], y <- [1..99]]
    print $ ps

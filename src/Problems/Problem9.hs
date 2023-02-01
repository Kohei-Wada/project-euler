module Problems.Problem9 where


import Control.Monad

pitagorean = [[x, y, z] | z <- [1..500], y <- [1..z], x <- [1..y], x*x + y*y == z*z]

problem9 = do 
    let triple = join $ filter (\t -> sum t == 1000)  pitagorean
    print $ product triple

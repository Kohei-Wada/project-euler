module Problems.Problem15 where


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1) 


routes :: Integer -> Integer
routes n = let a = factorial n 
            in factorial (n + n) `div` (a * a) 


problem15 :: IO () 
problem15 = print $ routes 20

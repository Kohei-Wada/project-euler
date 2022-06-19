module Problems.Problem48 where



l = [(x^x `mod` 10000000000) | x <- [1..1000]]


problem48 :: IO () 
problem48 = do 
    print $ sum l `mod` 10000000000

module Problems.Problem3 where 


primeFactors :: Int -> [Int]
primeFactors n = 
    let m = tmp n in 
        if m == n then [m] else [m] ++ primeFactors (n `div` m) 
    where 
        tmp n = head $ [ x | x <- [2..n], n `mod` x == 0]


problem3 :: IO () 
problem3 = do 
    let ans = maximum $ primeFactors 600851475143
    print ans
    


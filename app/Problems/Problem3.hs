module Problems.Problem3 where 

factor n = [x | x <- [1..n], n `mod` x == 0]



factorization :: Integer -> [Integer]
factorization 1 = []
factorization n = v : factorization (n `div` v) 
    where v = factor n !! 1



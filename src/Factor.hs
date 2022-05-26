module Factor where


factor n = [x | x <- [1..n], n `mod` x == 0]


factorization 1 = []
factorization n = v : factorization (n `div` v) 
    where v = factor n !! 1


--Todo 
isPrime :: Integer -> Bool
isPrime n 
  | n == 2 || n == 3 || n == 5 || n == 7 = True
  | n `mod` 2 == 0 = False  
  | n `mod` 3 == 0 = False
  | n `mod` 5 == 0 = False
  | n `mod` 7 == 0 = False
  | otherwise = factorization n == [n] 




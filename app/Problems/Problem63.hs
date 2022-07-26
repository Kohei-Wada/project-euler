module Problems.Problem63 where


order :: Integer -> Integer 
order 0 = 0 
order n = 1 + order (n `div` 10)


targetOf :: Integer -> [Integer]
targetOf n = filter (\y -> order y == n) $ takeWhile (\y -> order y <= n) [x^n | x <- [1..]]


problem63 :: IO () 
problem63 = do 
    let ans = length $ concat $ takeWhile (not. null) $ map targetOf [1..]
    print ans

module Problems.Problem53 where


comb :: Integer -> Integer -> Integer
comb n r = (factorial n) `div` (factorial r * factorial (n - r))
    where factorial 0 = 1
          factorial n = n * factorial (n - 1)


problem53 :: IO () 
problem53 = do 
    let l = [n `comb` r | n <- [23..100], r <- [1..n]]
        ans = length $ filter (> 1000000) l

    print ans

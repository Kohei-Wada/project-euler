module Problems.Problem5 where


lcmList :: (Integral a) => [a] -> a
lcmList l = foldl (lcm) 1 l


problem5 :: IO () 
problem5 = print $ lcmList [1..100000]

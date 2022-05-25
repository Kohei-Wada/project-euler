module Problems.Problem6 where



sigma n = n * (n + 1) `div` 2

sigmasquare n = n * (2*n + 1) * (n + 1) `div` 6

problem6 = print $ (sigma 100)^2 - sigmasquare 100

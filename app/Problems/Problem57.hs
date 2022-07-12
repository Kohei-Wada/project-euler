module Problems.Problem57 where


cFractionOf2 :: Integer -> [(Integer, Integer)]
cFractionOf2 n = map (\(x, y) -> (x - y, y)) (cf2 n)
    where 
        cf2 :: Integer -> [(Integer, Integer)]
        cf2 0 = [(1, 0)]
        cf2 1 = [(1, 0), (2, 1)]
        cf2 n = tmp n [(1, 0), (2, 1)]
            where 
                tmp 0 xs = xs
                tmp n xs = let l          = length xs
                               (p',  q' ) = xs!!(l-1)
                               (p'', q'') = xs!!(l-2)
                            in tmp (n-1) (xs ++ [(2*p' + p'', 2*q' + q'')])


order :: Integer -> Integer 
order n 
  | n < 10 = 1
  | otherwise = 1 + order (n `div` 10) 


problem57 :: IO () 
problem57 = do 
    print $ length $ filter (\(x, y) -> order x > order y) (cFractionOf2 1000)






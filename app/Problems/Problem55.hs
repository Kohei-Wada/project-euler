module Problems.Problem55 where


isBatch :: Integer -> Bool
isBatch n = show n == reverse (show n)


doLychrel :: Integer -> Integer
doLychrel n = n + (read . reverse . show) n


isLychrel :: Integer -> Bool
isLychrel l = tmp (l, 0) 
    where tmp (l, n) 
            | n < 50    = let l' = doLychrel l in if isBatch l' then False else tmp (l' , n + 1)
            | otherwise = True 


problem55 :: IO () 
problem55 = do 
    let ans = length $ filter isLychrel [1..10000]
    print ans


module Problems.Problem46 where


import Factors


check :: Integer -> Bool
check x = let l = [1 .. (fromIntegral . floor. sqrt . fromIntegral)(x `div` 2) ]
           in not $ null $ filter (\n -> isPrime (x-(2*n*n))) l 



problem46 :: IO () 
problem46 = do 
    let cmpns = filter (not . isPrime) [3, 5..]
    
    print $ head $ dropWhile check cmpns

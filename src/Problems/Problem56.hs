module Problems.Problem56 where



sumOfDigits :: Integer -> Integer
sumOfDigits 0 = 0
sumOfDigits n = n `mod` 10 + sumOfDigits (n `div` 10) 


problem56 :: IO ()
problem56 = do 
    let l = [a^b | a <- [1..100], b <- [1..100]]
    print $ maximum $ map sumOfDigits l 

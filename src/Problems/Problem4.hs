module Problems.Problem4 where



list = [a*b | a <- [100..999], b <- [100..999]]


isBatch :: (Show a) => a -> Bool
isBatch x = let tmp = show x in tmp == reverse tmp



problem4 :: IO () 
problem4 = print $ maximum $ filter isBatch list





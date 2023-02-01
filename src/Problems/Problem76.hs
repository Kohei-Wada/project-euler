module Problems.Problem76 where



partition :: Int -> Int
partition n
  | n < 0  = 0 
  | n == 0 = 1
  | otherwise = 1 + sum [tmp k (n - k) | k <- [1..(floor)(fromIntegral n / 2)]]

tmp k n 
  | k > n     = 0 
  | k == n    = 1
  | otherwise = tmp (k + 1) n + tmp k (n - k)




problem76 :: IO ()
problem76 = do 
    let ans = partition 100
    print ans
    

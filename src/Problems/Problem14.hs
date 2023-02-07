module Problems.Problem14 where

collatzLength :: Int -> Int
collatzLength n = go 1 n where
    go l n 
      | n == 1    = l 
      | otherwise = if even n then go (l + 1) (div n 2) else go (l + 1) (3 * n + 1) 

findLongest :: Int -> Int
findLongest n = go 1 1 1 where
    go cur len ans
      | cur == n  = ans
      | otherwise = 
          let len' = collatzLength cur  
           in if len < len' then go (cur + 1) len' cur else go (cur + 1) len ans

problem14 :: IO ()
problem14 = do 
    let ans = findLongest 1000000
    print ans 

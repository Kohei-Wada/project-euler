module Number where


order :: Int -> Int
order n 
  | n < 0     = undefined
  | n < 10    = 1
  | otherwise = 1 + order (n `div` 10)

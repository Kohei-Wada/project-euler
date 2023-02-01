module Problems.Problem31 where

type Amount = Integer
type Coin   = Integer


changes :: Amount -> [Coin] -> Integer
changes 0 _  = 1
changes _ [] = 0 
changes x ccs@(c:cs) 
  | x < 0     = 0
  | otherwise = changes (x - c) ccs + changes x cs


problem31 :: IO () 
problem31 = do 
    print $ changes 1000 [500, 100, 50, 10, 5, 1]

module Problems.Problem16 where
    
import Control.Monad


sumPlaces :: Integer -> Integer
sumPlaces 0 = 0
sumPlaces n = n `mod` 10 + sumPlaces (n `div` 10)


problem16 :: IO () 
problem16 = do 
    let a = 2 ^ 1000
    print $ sumPlaces a
    

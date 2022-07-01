module Problems.Problem38 where

import Data.List

catProducts :: Integer -> [Integer] -> Integer
catProducts n xs = read $ concat $ map (\x -> show (x*n)) xs :: Integer


isPandigital :: Integer -> Bool
isPandigital n = (sort . show) n == "123456789"


order :: Integer -> Integer
order = fromIntegral . length . show


pandigitals :: Integer -> [Integer]
pandigitals x = 
    let nss = [[1..n] | n <- [2..]] 
     in filter isPandigital $ takeWhile (\n -> order n <= 9) $ map (catProducts x) nss


problem38 :: IO () 
problem38 = do
    let pds = concat $ map (pandigitals) [1..10000]
    print $ maximum pds


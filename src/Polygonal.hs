module Polygonal where

type Polygonal = Int



triangle :: Int -> Polygonal
triangle n = n * (n + 1) `div` 2


pentagonal :: Int -> Polygonal
pentagonal n = n * (3 * n - 1) `div` 2


hexagonal :: Int -> Polygonal 
hexagonal n = n * (2 * n - 1)


isPentagonal :: Int -> Bool
isPentagonal n = let x = (1 + sqrt (1 + 24 * fromIntegral n)) / 6 :: Float
                  in x == realToFrac (truncate x)


isTriangular :: Int -> Bool
isTriangular n = let x = (-1 + sqrt (1 + 8 * fromIntegral n)) / 2 :: Float
                  in x == realToFrac (truncate x)


isHexagonal :: Int -> Bool
isHexagonal n = let x = (1 + sqrt (1 + 8 * fromIntegral n )) / 4 :: Float 
                 in x == realToFrac (truncate x)




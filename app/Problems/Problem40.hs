module Problems.Problem40 where

import Control.Monad


cham :: String
cham = concat [ show x | x <- [1..]]


problem40 :: IO () 
problem40 = do 
    let tmp = map (\n -> cham!!(n-1)) [10^n | n <- [0..7]]
    print $ product $ map (\x -> read [x] :: Int) tmp

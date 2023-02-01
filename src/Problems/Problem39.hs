{-# LANGUAGE TupleSections #-}
module Problems.Problem39 where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Function

pitagoreans = [[x, y, z] | x <- [1..1000]
                         , y <- [x..1000]
                         , z <- [y..1000]
                         , x*x + y*y == z*z
                         ]

targets = filter (\xs -> sum xs < 1000) pitagoreans

count = M.fromListWith (+) . map (,1)


problem39 :: IO () 
problem39 = do 
    let l =  M.toList $ count $ map sum targets
    print $ L.maximumBy (compare `on` snd) l


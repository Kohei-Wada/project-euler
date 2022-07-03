module Problems.Problem45 where

import Polygonal

{- if x is hexiagonal then x is triangular -}
problem45 :: IO () 
problem45 = do 
    let l   = [40756..]
        ans =  head $ filter (\x -> isHexagonal x && isPentagonal x) l

    print ans

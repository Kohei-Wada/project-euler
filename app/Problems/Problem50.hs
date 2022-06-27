module Problems.Problem50 where

import Eratosthenes
import Factors
import Control.Monad
import Data.Maybe


convert :: Integer -> [Integer] -> [[Integer]]
convert n as = let n' = fromIntegral n in 
                   if length as < n' || 2 * head as * n > last as 
                      then [] 
                      else [take n' as] ++ convert n (tail as) 


maxLength :: Integer -> Integer
maxLength n =  ((floor . sqrt) (1 + 8 * fromIntegral n) - 1) `div` 2


problem50 :: IO () 
problem50 = do 
    let n  = 1000000 :: Integer
        m  = (fromIntegral . maxLength) n
        ps = map fromIntegral $ erat [2 .. fromIntegral n]
        as = join $ 
            forM (reverse [21 .. m]) $ \l -> do 
                let pss = convert l ps
                    as  = filter (\t -> t < n && isPrime t) $ map sum pss
                return $ if null as then Nothing else Just (l, last as) 

    print $ head $ catMaybes as
    

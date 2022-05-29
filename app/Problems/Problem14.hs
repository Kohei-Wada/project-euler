{-# LANGUAGE RecordWildCards #-}

module Problems.Problem14 where

import Control.Monad
import Data.List


collatz :: Int -> [Int]
collatz 1 = []
collatz n = let a = if even n then n `div` 2 else 3 * n + 1 in a:collatz a



problem14 :: IO ()
problem14 = do 
    let list = map (length .collatz) [1..1000000]
    print $ maximum list
--    print $ length $ collatz 837799

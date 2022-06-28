module Problems.Problem26 where

import Quotient

import Control.Monad
import Data.List
import Data.Function


problem26 :: IO () 
problem26 = do 
    let qs = [ quotient (1, x) | x <- [1..1000]]
    print $ maximumBy (compare `on` snd) [(q, (length . repeatingPart) q) | q <- qs]

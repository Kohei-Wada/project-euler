{-# LANGUAGE RecordWildCards #-}

module Problems.Problem10 where

import Control.Monad.State
import Eratosthenes


problem10 :: IO () 
problem10 = do 
    print $ sum $ erat [2..2000000]

{-# LANGUAGE RecordWildCards #-}

module Eratosthenes where

import Control.Monad.State


data Erat = Erat 
    { _nums   :: ![Int] 
    , _primes :: ![Int]
    , _range  :: !Int
    } deriving Show


makeErat :: [Int] -> Erat
makeErat l = Erat l [] ((floor . sqrt . fromIntegral . length) l)


doErat :: State Erat () 
doErat = do 
    e@Erat{..} <- get 
    let newPrime = head _nums
    if (_range > newPrime) 
       then do 
           put $ e { _nums = filter (\x -> x `mod` newPrime /= 0) _nums 
                   , _primes = _primes ++ [newPrime]
                   }
           doErat 
       else do 
           put $ e { _primes = _primes ++ _nums 
                   , _nums = []
                   }
           return ()


erat' :: [Int] -> [Int]
erat' l = let Erat{..} = execState doErat $ makeErat l in _primes


erat :: Int -> [Int]
erat n = erat' [2..n]


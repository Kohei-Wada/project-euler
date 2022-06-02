module Fibonacci where

import Data.Semigroup
import Data.Monoid

-- FibPair a b : (a, b) = (F[n], F[n+1]) for some n
data FibPair a = FibPair !a !a deriving (Eq,Show)


instance (Num a) => Semigroup (FibPair a) where
  FibPair a b <> FibPair a' b'
    = FibPair (a * b' + (b - a) * a') (a * a' + b * b')
  stimes = stimesMonoid


instance (Num a) => Monoid (FibPair a) where
  -- (F[0], F[1])
  mempty = FibPair 0 1


-- fibOne = (F[1], F[2])
fibOne :: (Num a) => FibPair a
fibOne = FibPair 1 1


-- fibPair i = (F[i], F[i+1])
fibPair :: Int -> FibPair Integer
fibPair i = stimesMonoid i fibOne


fib :: Int -> Integer
fib i = case fibPair i of
          FibPair a _ -> a



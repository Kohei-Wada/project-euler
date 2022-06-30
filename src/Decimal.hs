{-# LANGUAGE RecordWildCards #-}
module Decimal where

import Data.Ratio
import GHC.Real


newtype Decimal a = Decimal (Ratio a)


instance (Show a, Eq a, Integral a) => Show (Decimal a) where 
    show (Decimal x) = "Decimal " ++
        if isRepeating x 
           then let fp  = fractionalPart x 
                    rp  = repeatingPart x
                    tmp = takeWhile (/= head rp) fp in 
                    show (integerPart x) ++ "." ++ 
                        concat (map show tmp) ++ "(" ++ (concat (map show rp)) ++ ")"

           else let fp = init $ fractionalPart x in 
                    show (integerPart x) ++ "." ++ (concat (map show fp))


toDecimal :: Ratio a -> Decimal a
toDecimal = Decimal


isRepeating :: (Integral a) => Ratio a -> Bool 
isRepeating = not . null . repeatingPart


repeatingPart :: (Eq a, Integral a) =>  Ratio a -> [a]
repeatingPart x = 
    let fp = fractionalPart x 
     in if last fp == 0 && last (init fp) == 0 
           then [] else dropWhile (/= last fp) (init fp) 
                     

integerPart :: Integral a => Ratio a -> a
integerPart (a :% b) = a `div` b


fractionalPart :: (Integral a) => Ratio a -> [a]
fractionalPart (a :% b) = 
     map (\x -> 10*x `div` b) $ reclist b [a `mod` b]
        where 
            reclist b cs = 
                let l   = last cs
                    rem = l*10 `mod` b
                 in 
                    if l == 0 || elem rem cs
                       then cs ++ [rem] else reclist b (cs ++ [rem])


quotientTest :: IO () 
quotientTest = do 
    let target = [ 1 % x | x <- [1, 3, 11, 27, 101, 41, 7, 239, 73, 81, 451, 21649, 707, 53]]
    print $ map (length . repeatingPart) target == [0..13] 


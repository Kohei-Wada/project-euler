{-# LANGUAGE RecordWildCards #-}
module Problems.Problem26 where

import Control.Monad
import Data.List
import Data.Function

type Q = (Integer, Integer)

data Decimal = Undefinde | Decimal
    { _q :: Q
    , _pinteger :: Integer
    , _pfractional :: [Integer]
    }


instance Show Decimal where
    show Decimal{..} = show _pinteger ++ "." ++  (concat $ map show _pfractional) 


q2decimal :: Q -> Decimal 
q2decimal (_, 0) = Undefinde
q2decimal x      = Decimal x (integerPart x) (fractionalPart x) 


repeatingPart :: Q -> [Integer]
repeatingPart x = 
    let fp = fractionalPart x 
     in if last fp == 0 && last (init fp) == 0 
           then []
           else dropWhile (/= last fp) (init fp) 
                     

integerPart :: Q -> Integer
integerPart (a, b) = a `div` b


fractionalPart :: Q -> [Integer]
fractionalPart x = 
    let (a, b) = reducing x 
        r = a `mod` b 
     in map (\x -> 10*x `div` b) $ reclist b [r]
        where 
            reclist :: Integer -> [Integer] -> [Integer]
            reclist _ [] = []
            reclist b cs = 
                let l   = last cs
                    rem = l*10 `mod` b
                 in 
                    if l == 0 || elem rem cs
                       then cs ++ [rem]
                       else reclist b (cs++[rem])

--TODO
reducing :: Q -> Q 
reducing x = x


test :: IO () 
test = do 
    let target = [(1, x) | x <- [1, 3, 11, 27, 101, 41, 7, 239, 73, 81, 451, 21649, 707, 53]]
    forM_ target (print . length . repeatingPart)



problem26 :: IO () 
problem26 = do 
    let qs = [(1, x) | x <- [1..1000]]
    print $ maximumBy (compare `on` snd) [(q, (length . repeatingPart) q) | q <- qs]

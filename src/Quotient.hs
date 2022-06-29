{-# LANGUAGE RecordWildCards #-}
module Quotient where

import Control.Monad

newtype Quotient = Quotient (Integer, Integer)  

data Decimal = Decimal { _quo :: Quotient }


instance Num Quotient where 
    Quotient (a, b) + Quotient (c, d) = Quotient (a*d + b*c, b*d) 
    Quotient (a, b) - Quotient (c, d) = Quotient (a*d - b*c, b*d) 
    Quotient (a, b) * Quotient (c, d) = Quotient (a*c, b*d) 
    abs (Quotient (a, b))             = Quotient (abs a, abs b) 
    negate (Quotient (a, b))          = Quotient (negate a, b)
    fromInteger a                     = Quotient (a, 1)
    signum a                          = 1


instance Show Quotient where
    show (Quotient (a, b)) = "Quotient " ++ show a ++ "/" ++ show b


instance Show Decimal where 
    show Decimal{..} = "Decimal " ++
        if isRepeating _quo 
           then let fp  = fractionalPart _quo 
                    rp  = repeatingPart _quo 
                    tmp = takeWhile (/= head rp) fp in 
                    show (integerPart _quo) ++ "." ++ 
                        concat (map show tmp) ++ "(" ++ (concat (map show rp)) ++ ")"

           else let fp = init $ fractionalPart _quo in 
                    show (integerPart _quo) ++ "." ++ (concat (map show fp))


q2d :: Quotient -> Decimal 
q2d = Decimal


quotient :: (Integer, Integer) -> Quotient
quotient = Quotient 


isRepeating :: Quotient -> Bool 
isRepeating = not . null . repeatingPart


repeatingPart :: Quotient -> [Integer]
repeatingPart x = 
    let fp = fractionalPart x 
     in if last fp == 0 && last (init fp) == 0 
           then [] else dropWhile (/= last fp) (init fp) 
                     

integerPart :: Quotient -> Integer
integerPart (Quotient (a, b)) = a `div` b


fractionalPart :: Quotient -> [Integer]
fractionalPart (Quotient (a, b)) = 
     map (\x -> 10*x `div` b) $ reclist b [a `mod` b]
        where 
            reclist b cs = 
                let l   = last cs
                    rem = l*10 `mod` b
                 in 
                    if l == 0 || elem rem cs
                       then cs ++ [rem] else reclist b (cs ++ [rem])

--TODO
reduce:: Quotient -> Quotient 
reduce = id


quotientTest :: IO () 
quotientTest = do 
    let target = [ Quotient (1, x) | x <- [1, 3, 11, 27, 101, 41, 7, 239, 73, 81, 451, 21649, 707, 53]]
    print $ map (length . repeatingPart) target == [0..13] 


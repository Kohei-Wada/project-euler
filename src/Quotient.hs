{-# LANGUAGE RecordWildCards #-}
module Quotient where

import Control.Monad

data Quotient = Quotient 
    { _q       :: (Integer, Integer) } 


data Decimal = Decimal 
    { _quo :: Quotient }


instance Show Quotient where
    show Quotient{..} = let (a, b) = _q in show a ++ "/" ++ show b


instance Show Decimal where 
    show Decimal{..} = 
        if isRepeating _quo 
           then let fp  = fractionalPart _quo 
                    rp  = repeatingPart _quo 
                    tmp = takeWhile (/= head rp) fp in 
                    show (integerPart _quo) ++ "." ++ 
                        concat (map show tmp) ++ "(" ++ (concat (map show rp)) ++ ")"

           else let fp = init $ fractionalPart _quo in 
                    show (integerPart _quo) ++ "." ++ (concat (map show fp))


q2d :: Quotient -> Decimal 
q2d x@Quotient{..} = Decimal x


quotient :: (Integer, Integer) -> Quotient
quotient = Quotient 


isRepeating :: Quotient -> Bool 
isRepeating = not . null . repeatingPart


repeatingPart :: Quotient -> [Integer]
repeatingPart x@Quotient{..} = 
    let fp = fractionalPart x 
     in if last fp == 0 && last (init fp) == 0 
           then [] else dropWhile (/= last fp) (init fp) 
                     

integerPart :: Quotient -> Integer
integerPart x@Quotient{..} = let (a, b) = _q in a `div` b


fractionalPart :: Quotient -> [Integer]
fractionalPart x@Quotient{..} = 
    let (a, b) = _q   
     in map (\x -> 10*x `div` b) $ reclist b [a `mod` b]
        where 
            reclist b cs = 
                let l   = last cs
                    rem = l*10 `mod` b
                 in 
                    if l == 0 || elem rem cs
                       then cs ++ [rem]
                       else reclist b (cs ++ [rem])

--TODO
reducing :: Quotient -> Quotient 
reducing = id


qtientTest :: IO () 
qtientTest = do 
    let target = [ Quotient (1, x) | x <- [1, 3, 11, 27, 101, 41, 7, 239, 73, 81, 451, 21649, 707, 53]]
    print $ map (length . repeatingPart) target == [0..13] 


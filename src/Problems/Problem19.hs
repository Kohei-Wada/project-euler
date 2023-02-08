module Problems.Problem19 where

import Data.List (foldl') 
import Control.Monad (forM_)

type Day = Int
type Month = Int
type Year = Int

data Dow = Sun|Mon|Tue|Wed|Thu|Fri|Sur deriving (Show, Eq, Enum)  -- Day Of Week 

data Date = Date Day Month Year Dow deriving (Show)

daysOfMonth :: Month -> Year -> Int
daysOfMonth 2 y
  | y `mod` 400 /= 0 && y `mod` 100 == 0 = 28
  | y `mod` 4 == 0 = 29
  | otherwise = 28
daysOfMonth m y 
  | m == 4 || m == 6 || m == 9 || m == 11 = 30
  | otherwise = 31


nextDow :: Dow -> Dow
nextDow w = toEnum $ (fromEnum w + 1) `mod` 7 


nextDate :: Date -> Date
nextDate (Date d m y w) = 
    let nd = daysOfMonth m y 
        tmp = (d + 1) `mod` (nd + 1)
        d' = if tmp == 0 then 1 else tmp 

        tmp' = if tmp == 0 then (m + 1) `mod` (12 + 1) else m
        m' = if tmp' == 0 then 1 else tmp'

        y' = if tmp' == 0 then y + 1 else y 
        w' = nextDow w
     in Date d' m' y' w'


dateFrom1900 :: [Date]
dateFrom1900 = iterate nextDate (Date 1 1 1900 Mon)


year :: Date -> Year
year (Date _ _ y _) = y


problem19 :: IO () 
problem19 = do 
    let days = takeWhile ((/= 2001) . year) $ dropWhile ((/= 1901) . year) dateFrom1900
        ans = foldl' countTarget 0 days
            where 
                countTarget n (Date d _ _ w) = if d == 1 && w == Sun then n + 1 else n 

    print ans

module Problems.Problem19 where


data DOTW = Sun|Mon|Tue|Wed|Thu|Fri|Sur deriving (Show, Eq) 



dayOfTheWeek :: Int -> DOTW
dayOfTheWeek 0 = Sun
dayOfTheWeek 1 = Mon
dayOfTheWeek 2 = Tue
dayOfTheWeek 3 = Wed 
dayOfTheWeek 4 = Thu 
dayOfTheWeek 5 = Fri 
dayOfTheWeek 6 = Sur 


isLeapYear :: Int -> Bool
isLeapYear n = not $ n `mod` 4 /= 0 || n `mod` 400 /= 0 && n `mod` 100 == 0


daysFrom1900 :: Int -> Int
daysFrom1900 n  
  | n <  1900 = undefined 
  | n == 1900 = 0 
  | otherwise = (sum $ map (\x -> if isLeapYear x then 366 else 365) [1900..n]) - 365


problem19 :: IO () 
problem19 = do 
    let years = [1900..2000]
        weeks = map (\x -> dayOfTheWeek (x `mod` 7)) $ (map daysFrom1900 years)

    print $  filter (\w -> w == Sun) weeks





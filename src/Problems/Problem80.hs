module Problems.Problem80 where

import Data.Char


squareRoot :: String -> String
squareRoot cs = root' (int' ++ frac' ++ repeat '0') 0 0
  where
    (int, frac) = break (== '.') cs
    int'  = if odd (length int) then ('0' : int) else int
    frac' = if null frac then "." else frac
    root' ('.' : xs) r d   = '.' : root' xs r d
    root' (a : b : xs) r d = (intToDigit . fromIntegral) n' : root' xs r' d'
      where
        n  = r * 100 + read [a, b]
        n' = last $ takeWhile (\x -> (d + x) * x <= n) [0 .. 9]
        r' = n - (d + n') * n'
        d' = 10 * (d + n' + n')


f :: Int -> Bool
f n =  let tmp = (fromIntegral . floor . sqrt . fromIntegral) n in tmp^2 /= n



sumDigitsOf :: Int -> Int -> Int 
sumDigitsOf n x = 
    let tmp = take n $ squareRoot $ show x

     in sum $ map (\d -> if d == '.' then 0 else (read . pure) d) tmp




problem80 :: IO () 
problem80 = do 
    let tmp = map (sumDigitsOf 101)  $ filter f [1..100]
        ans = sum tmp 

    print ans

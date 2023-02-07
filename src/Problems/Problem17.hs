module Problems.Problem17 where

numList = ["", "one", "two", "three", "four", "five", "six", "seven"
          , "eight", "nine", "ten", "eleven", "twelve", "thirteen" 
          , "fourteen", "fifteen", "sixteen", "seventeen", "eighteen"
          , "nineteen"
          ]

numList' = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy"
           , "eighty", "ninety"
           ]

numList'' = ["hundred", "hundred and"]

n2s :: Int -> String
n2s n
  | n == 0                   = ""
  | (0   <  n) && (n < 20)   = numList  !! n 
  | (20  <= n) && (n < 100)  = numList' !! (maxPlace n) ++ n2s (n `mod` 10)
  | (100 <= n) && (n < 1000) = numList  !! (maxPlace n) ++ "hundred" ++ 
      let rem = n `mod` 100 in if rem == 0 then "" else "and" ++ n2s rem
  | otherwise = "onethousand"

    where maxPlace n = (read $ [head $ show n]) :: Int


problem17 :: IO () 
problem17 = do 
    let ans = length $ concat $ map n2s [1 .. 1000]
    print ans

module Problems.Problem74 where


import Control.Parallel.Strategies 
import Control.DeepSeq
import Data.List.Split
import Control.Monad


factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1) 


chain :: Integer -> [Integer]
chain n = [n] ++ chain' n 
    where 
        chain' n = let tmp = next n in if n == tmp then [tmp] else [tmp] ++ chain' tmp
        next 0 = 0 
        next n = factorial (n `mod` 10) + next (n `div` 10)


cycleOfChain :: [Integer] -> [Integer]
cycleOfChain as = 
    let 
        loop :: [Integer] -> [Integer] -> [Integer] 
        loop _ []     = []
        loop es (a:as) =  if a `elem` es then es else loop (es ++ [a]) as
     in 
        loop [] as 


solve = length . cycleOfChain . chain 


solve1 :: IO () 
solve1 = do
    let targets = [1..1000000]
        cs = chunksOf (length targets `div` 16) targets

        soltions = runEval $ do 
            cs' <- forM cs (\c -> rpar $ force $ map solve c)
            forM_ cs' rseq 
            return $ concat cs'

        ans = length $ filter (==60) soltions 

    print ans


problem74 :: IO () 
problem74 = solve1

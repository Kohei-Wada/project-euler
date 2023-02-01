module Problems.Problem58 where

import Data.Numbers.Primes (isPrime)

type EdgeLength  = Int


corners :: EdgeLength -> [Int]
corners l = let tmp = l^2 in  [tmp - (l-1), tmp - 2*(l-1), tmp - 3*(l-1)]


--N of Prime Corners
npc :: EdgeLength -> Int
npc = length . (filter isPrime) . corners


triples :: [(EdgeLength, Int, Int)]
triples = scanl (\(_ ,cd, cp) l -> (l, cd + 4, cp + npc l)) (1, 1, 0) edges 
    where edges = [3, 5..]


toRatio :: (EdgeLength, Int, Int) -> Float
toRatio (_, cd, cp) = fromIntegral cp / fromIntegral cd


problem58 :: IO () 
problem58 = do 
    let (ans, _, _) = head $ dropWhile (\t -> toRatio t > 0.1) $ tail triples
    print ans

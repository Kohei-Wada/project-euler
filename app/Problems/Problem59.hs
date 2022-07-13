module Problems.Problem59 where

import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Control.Monad


type Key    = [Int]
type Cipher = [Int]


decrypt :: Key -> [Int] -> [Int] 
decrypt k ns = let nss = chunksOf 3 ns in concat $ map (tmp k) nss
    where 
        tmp :: [Int] -> [Int] -> [Int]
        tmp _ []          = []
        tmp [] _          = []
        tmp (k:ks) (c:cs) = [k `xor` c] ++ tmp ks cs


problem59 :: IO () 
problem59 = do 
    let f = "data/p059_cipher.txt"
        cs = ['a'..'z']
        keys = [map ord [a0, a1, a2] | a0 <- cs, a1 <- cs, a2 <- cs] 

    tmp <- readFile f 

    let cipher = map (\s -> read s :: Int)  (splitOn "," tmp) 

        correctKey = head $ filter (\k -> 
            let s = map chr $ decrypt k cipher 
             in "the" `isInfixOf` s && "is " `isInfixOf` s && "by " `isInfixOf` s 
            ) keys 
    
    

        ans = sum correctKey 
        
    print ans

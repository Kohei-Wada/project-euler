{-# LANGUAGE OverloadedStrings #-}
module Problems.Problem22 where

import Data.List (sort) 
import Data.Char (ord) 

import qualified Data.Text as T
import Data.Text (Text)

isTarget :: Char -> Bool
isTarget '\"' = False
isTarget '\n' = False 
isTarget _ = True

names2score :: [Text] -> [Int]
names2score ns = go ns 1 where
    go [] _     = []
    go (n:ns) i = i * name2score n : go ns (i + 1)  

name2score :: Text -> Int
name2score = T.foldl (\acc c -> acc + (ord c - 64)) 0 

problem22 :: IO () 
problem22 = do 
    d <- readFile "data/names.txt" 
    let ns = sort $ T.splitOn "," $ T.filter isTarget $ T.pack d
        ss = names2score ns 
        ans = sum ss 

    print ans

module Problems.Problem22 where
    
import Data.List
import Data.Char




namesList :: String -> [String]
namesList s = filter (not . null) $ lines $ parser s
    where 
        parser :: String -> String 
        parser = map $ \x -> 
            case x of 
              ',' -> '\n'
              '"' -> '\n'
              otherwise -> x
              


name2score :: String -> Int
name2score n = 
    foldl (\s x -> s + ord x - 64) 0 n



problem22 :: IO () 
problem22 = do 
    d <- readFile "data/names.txt" 
    print $ sum $ map name2score $ sort $ namesList d

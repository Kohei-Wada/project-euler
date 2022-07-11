module Problems.Problem54 where

import Data.List.Split
import Data.List
import Data.Function
import Data.List.Unique

data Suit = D | H | C | S deriving (Show, Eq)

type Rank = Int 

data Card = Card 
    { _rank :: Rank 
    , _suit :: Suit 
    } deriving Eq


instance Show Card where
    show (Card r s) = show r ++ show s

instance Ord Card where
    compare (Card r _) (Card r' _) = compare r r'


s2c :: String -> Card
s2c (r:s) = let rnk = case r of 
                        'A' -> 1
                        '2' -> 2
                        '3' -> 3
                        '4' -> 4
                        '5' -> 5
                        '6' -> 6
                        '7' -> 7
                        '8' -> 8
                        '9' -> 9
                        'T' -> 10
                        'J' -> 11
                        'Q' -> 12
                        'K' -> 13

                st  = case head s of 
                        'D' -> D
                        'H' -> H
                        'C' -> C
                        'S' -> S
         in Card rnk st


data Hand = HighCard Int
          | OnePair 
          | TwoPair 
          | ThreeCard 
          | Straight 
          | Flash 
          | FullHouse 
          | FourCard 
          | StraightFlash 
          | RoyalFlash
          deriving (Show, Eq, Ord)


cs2h :: [Card] -> Hand
cs2h cs 
  | isRoyalFlash cs    = RoyalFlash
  | isStraightFlash cs = StraightFlash
  | isFourCard cs      = FourCard
  | isFullHouse cs     = FullHouse
  | isFlash cs         = Flash
  | isStraight cs      = Straight
  | isThreeCard cs     = ThreeCard
  | isTwoPair cs       = TwoPair
  | isOnePair cs       = OnePair
  | otherwise          = HighCard (highCard cs)
  where isRoyalFlash :: [Card] -> Bool
        isRoyalFlash cs = isFlash cs && map (_rank) cs == [1, 10, 11, 12, 13]

        isStraightFlash :: [Card] -> Bool
        isStraightFlash cs  = isStraight cs && isFlash cs

        isFourCard :: [Card] -> Bool
        isFourCard cs = let l = count (map _rank cs) in any (\(v, c) -> c == 4) l 

        isFullHouse :: [Card] -> Bool
        isFullHouse cs = let l = count (map _rank cs) in any (\(v, c) -> c == 3) l && any (\(v, c) -> c == 2) l

        isFlash :: [Card] -> Bool
        isFlash (c:cs) = let Card r s = c in all (\c -> _suit c == s) cs 

        isStraight :: [Card] -> Bool 
        isStraight cs = 
            let l = map _rank cs in tmp l 
                where tmp (n0:ns) = if ns == [] then True else (n0 + 1) == head ns && tmp ns

        isThreeCard :: [Card] -> Bool 
        isThreeCard cs = let l = count (map _rank cs) in any (\(v, c) -> c == 3) l 

        isTwoPair :: [Card] -> Bool
        isTwoPair cs = length (count (map _rank cs)) == 3

        isOnePair :: [Card] -> Bool
        isOnePair cs = let l = count (map _rank cs) in any (\(v, c) -> c == 2) l 

        highCard :: [Card] -> Rank
        highCard cs = let Card r s = last cs in r 



type Winner = Int
doGame :: [Card] -> Winner 
doGame cs = 
    let p1 = sortBy (compare `on` _rank) $ take 5 cs 
        p2 = sortBy (compare `on` _rank) $ drop 5 cs
     in if cs2h p1 > cs2h p2 then 1 else 2


problem54 :: IO () 
problem54 = do 
    s <- readFile "data/p054_poker.txt"
    let gs = map (map s2c) $ map (splitOn " ") $ lines s
        --p1 = map (\g -> sortBy (compare `on` _rank) $ take 5 g) gs

        ans = length $ filter (==1) $ map doGame gs 

    print ans


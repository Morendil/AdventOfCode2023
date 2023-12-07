import Data.Char (digitToInt, isDigit)
import Data.List (sort, group)

newtype Hand = Hand String deriving (Eq, Show)
instance Ord Hand where
    compare (Hand s1) (Hand s2) = compare (classify s1) (classify s2) <> compare (map value s1) (map value s2)

parseHand :: String -> (Hand, Int)
parseHand h = (Hand hand,read bid) where [hand,bid] = words h

value :: Char -> Int
value c | isDigit c = digitToInt c
value 'T' = 10
value 'J' = 11
value 'Q' = 12
value 'K' = 13
value 'A' = 14

classify :: String -> Int
classify = byType . sort . map length . group . sort
    where byType [5] = 7
          byType [1,4] = 6
          byType [2,3] = 5
          byType [1,1,3] = 4
          byType [1,2,2] = 3
          byType [1,1,1,2] = 2
          byType _ = 1

part1 :: [(Hand, Int)] -> Int
part1 = sum . zipWith (*) [1..] . map snd . sort

main = do
    hands <- map parseHand . lines <$> readFile "day07.txt"
    print $ part1 hands
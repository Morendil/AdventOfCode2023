import Data.Char (digitToInt, isDigit)
import Data.List (sort, group)

newtype Hand = Hand String deriving (Eq, Show)
instance Ord Hand where
    compare (Hand s1) (Hand s2) = compare (classify s1) (classify s2) <> compare (map value s1) (map value s2)

newtype JokerHand = JokerHand String deriving (Eq, Show)
instance Ord JokerHand where
    compare (JokerHand s1) (JokerHand s2) = compare (jokerClassify s1) (jokerClassify s2) <> compare (map jokerValue s1) (map jokerValue s2)

parseHand :: String -> (Hand, Int)
parseHand h = (Hand hand,read bid) where [hand,bid] = words h

parseJokerHand :: String -> (JokerHand, Int)
parseJokerHand h = (JokerHand hand,read bid) where [hand,bid] = words h

value :: Char -> Int
value c | isDigit c = digitToInt c
value 'T' = 10
value 'J' = 11
value 'Q' = 12
value 'K' = 13
value 'A' = 14

jokerValue :: Char -> Int
jokerValue c | isDigit c = digitToInt c
jokerValue 'J' = 1
jokerValue 'T' = 10
jokerValue 'Q' = 11
jokerValue 'K' = 12
jokerValue 'A' = 13

jokerClassify :: String -> Int
jokerClassify "JJJJJ" = 7
jokerClassify hand = (byType . reverse . zipWith (+) [countJ,0,0,0,0] . reverse . sort . map length . group . sort) removeJ
    where removeJ = filter ('J' /=) hand
          countJ = length $ filter ('J' ==) hand

classify :: String -> Int
classify = byType . sort . map length . group . sort

byType :: [Int] -> Int
byType [5] = 7
byType [1,4] = 6
byType [2,3] = 5
byType [1,1,3] = 4
byType [1,2,2] = 3
byType [1,1,1,2] = 2
byType _ = 1

part1 :: [String] -> Int
part1 = sum . zipWith (*) [1..] . map snd . sort . map parseHand

part2 :: [String] -> Int
part2 = sum . zipWith (*) [1..] . map snd . sort . map parseJokerHand

main = do
    hands <- lines <$> readFile "day07.txt"
    print $ part1 hands
    print $ part2 hands
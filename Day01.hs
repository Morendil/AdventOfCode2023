import Data.Char
import Data.List
import Data.Maybe
import Test.Hspec

digitsMap = [("one",1),("two",2),("three",3),("four",4),("five",5),("six",6),("seven",7),("eight",8),("nine",9)]

startsWithWhatDigit :: String -> Maybe Int
startsWithWhatDigit (c:rest) | isDigit c = Just $ digitToInt c
startsWithWhatDigit s = fmap snd $ find (\(name,value) -> name `isPrefixOf` s) digitsMap

toDigits :: String -> [Int]
toDigits = mapMaybe startsWithWhatDigit . tails

digits :: String -> String
digits = filter isDigit

firstAndLast :: [a] -> [a]
firstAndLast l = head l : [last l]

value :: String -> Int
value = read . firstAndLast . digits

otherValue :: String -> Int
otherValue = read . concatMap show . firstAndLast . toDigits

part1 :: [String] -> Int
part1 = sum . map value

part2 :: [String] -> Int
part2 = sum . map otherValue

main = do
    calibration <- lines <$> readFile "day01.txt"
    print $ part1 calibration
    print $ part2 calibration

test :: IO ()
test = hspec $ do
  describe "Overlapping substitutions" $ do
    it "Higher overlaps with lower" $
        toDigits "twone" `shouldBe` [2,1]
    it "Lower overlaps with higher" $
        toDigits "oneight" `shouldBe` [1,8]
    it "Matches a digit" $
        startsWithWhatDigit "1three" `shouldBe` Just 1

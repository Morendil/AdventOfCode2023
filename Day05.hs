import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Map as M
import Test.Hspec

type Seed = Int
type Map = [[Int]]
type Almanac = ([Seed],[Map])

almanac :: ReadP Almanac
almanac = do
    string "seeds: "
    seeds <- sepBy1 number (string " ")
    string "\n\n"
    maps <- sepBy1 rangeMap (string "\n\n")
    return (seeds, maps)
rangeMap = do
    many1 (satisfy (/=':'))
    string ":\n"
    sepBy1 (sepBy1 number (string " ")) (string "\n")
number = read <$> many1 (satisfy isNumber)

transform :: Int -> [Int] -> Maybe Int
transform from [dest,src,len] = if from >= src && from < src+len then Just (from+dest-src) else Nothing

transformAll :: Int -> Map -> Int
transformAll from ranges = if null attempts then from else head attempts
    where attempts = mapMaybe (transform from) ranges

cascade :: Int -> [Map] -> Int
cascade = foldl transformAll

part1 :: Almanac -> Int
part1 (seeds, ranges) = minimum $ map (`cascade` ranges) seeds

main = do
    almanac <- fromJust . parseMaybe almanac <$> readFile "day05.txt"
    print $ part1 almanac

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

test :: IO ()
test = hspec $ do
  describe "Range mapping" $ do
    it "Transform in range" $ do
        transform 79 [50,98,2] `shouldBe` Nothing
        transform 97 [50,98,2] `shouldBe` Nothing
        transform 98 [50,98,2] `shouldBe` Just 50
        transform 99 [50,98,2] `shouldBe` Just 51
        transform 100 [50,98,2] `shouldBe` Nothing
        transform 79 [52,50,48] `shouldBe` Just 81

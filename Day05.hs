import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (fromJust, mapMaybe)
import Data.List.Extra
import qualified Data.Map as M
import Test.Hspec

type Seed = Int
type Shift = [Int]
type Map = [Shift]
type Almanac = ([Seed],[Map])
type Range = (Int,Int)

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

transform :: Int -> Shift -> Maybe Int
transform from [dest,src,len] = if from >= src && from < src+len then Just (from+dest-src) else Nothing

transformRange :: Range -> Shift -> ([Range], [Range])
transformRange (from,count) [dest,src,len] = (within,below++above)
    where overlap = (from < src+len) && (from+count > src)
          within = if overlap then let start = max from src in [(start+dest-src, min (from+count) (src+len) - start)] else []
          above = if from+count > src+len then let start = max from (src+len) in [(start,from+count-start)] else []
          below = if from < src then let start = min from src in [(start,min (from+count) src-start)] else []

transformRangeAll :: Range -> Map -> [Range]
transformRangeAll range [] = [range]
transformRangeAll range (shift:rest) = mapped ++ concatMap (`transformRangeAll` rest) unmapped
    where (mapped, unmapped) = transformRange range shift

transformAll :: Int -> Map -> Int
transformAll from ranges = if null attempts then from else head attempts
    where attempts = mapMaybe (transform from) ranges

cascade :: Int -> [Map] -> Int
cascade = foldl transformAll

part1 :: Almanac -> Int
part1 (seeds, maps) = minimum $ map (`cascade` maps) seeds

part2 :: Almanac -> Int
part2 (seeds, maps) = minimum $ map fst $ foldl (\seedRanges rangeMap -> concatMap (`transformRangeAll` rangeMap) seedRanges) seedRanges maps
    where seedRanges = map (\[a,b] -> (a,b)) $ chunksOf 2 seeds

main = do
    almanac <- fromJust . parseMaybe almanac <$> readFile "day05.txt"
    print $ part1 almanac
    print $ part2 almanac

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
    it "Transform range" $ do
        transformRange (79,14) [50,98,2] `shouldBe` ([],[(79,14)])
        transformRange (79,14) [52,50,48] `shouldBe` ([(81,14)],[])
        transformRange (79,14) [52,50,40] `shouldBe` ([(81,11)],[(90,3)])
        transformRange (50,50) [81,79,14] `shouldBe` ([(81,14)],[(50,29),(93,7)])
    it "Transform range with map" $ do
        transformRangeAll (79,14) [[50,98,2],[52,50,48]] `shouldBe` [(81,14)]
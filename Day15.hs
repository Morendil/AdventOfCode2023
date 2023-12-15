import Data.Text (pack, unpack, splitOn)
import Data.Char (ord, isAlpha, isNumber)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Foldable (toList)
import Control.Monad (join)
import qualified Data.IntMap as I
import qualified Data.Sequence as S

type Boxes = I.IntMap (S.Seq String)

hashOne :: Int -> Char -> Int
hashOne current c = (current + ord c) * 17 `mod` 256

hash :: String -> Int
hash = foldl hashOne 0

part1 :: [String] -> Int
part1 = sum . map hash

processOne :: Boxes -> String -> Boxes
processOne boxes step
        | isRemove && isJust pos = I.insert box (S.deleteAt (fromJust pos) (fromJust lenses)) boxes
        | isJust pos = I.insert box (S.update (fromJust pos) step (fromJust lenses)) boxes
        | not isRemove = I.insert box ((S.|>) (fromMaybe S.empty lenses) step) boxes
        | otherwise = boxes
    where label = filter isAlpha
          isRemove = last step == '-'
          stepLabel = label step
          box = hash stepLabel
          pos = S.findIndexL (\e -> label e == stepLabel) =<< lenses
          lenses = I.lookup box boxes

focusingPower :: S.Seq String -> [Integer]
focusingPower = toList . S.mapWithIndex value
    where value i s = fromIntegral (i+1) * read (filter isNumber s)

part2 :: [String] -> Integer
part2 = sum . concatMap power . I.assocs . foldl processOne I.empty
    where power (i,lenses) = map (*(fromIntegral $ i+1)) $ focusingPower lenses

main = do
    initialization <- map unpack . splitOn (pack ",") . pack . head . lines <$> readFile "day15.txt"
    print $ part1 initialization
    print $ part2 initialization
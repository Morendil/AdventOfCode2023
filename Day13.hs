import Data.Text (pack, unpack, splitOn)
import Data.List (transpose)

reflectsAbout :: Int -> String -> Bool
reflectsAbout n s = and $ zipWith (==) (reverse $ take n s) (drop n s)
    where l = length s

axes :: [String] -> [Int]
axes pattern = filter isAxis [1..l-1]
    where l = length $ head pattern
          isAxis n = all (reflectsAbout n) pattern

value :: [String] -> Int
value pattern = sum (axes pattern) + 100 * sum (axes $ transpose pattern)

part1 :: [[String]] -> Int
part1 = sum . map value

main = do
    patterns <- map (lines . unpack) . splitOn (pack "\n\n") . pack <$> readFile "day13.txt"
    print $ part1 patterns
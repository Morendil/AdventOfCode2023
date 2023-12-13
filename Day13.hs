import Data.Text (pack, unpack, splitOn)
import Data.List (transpose)

reflectAbout :: Int -> String -> [Bool]
reflectAbout n s = zipWith (/=) (reverse $ take n s) (drop n s)
    where l = length s

axes :: Int -> [String] -> [Int]
axes smudges pattern = filter isAxis [1..l-1]
    where l = length $ head pattern
          isAxis n = smudges == length (filter id $ concatMap (reflectAbout n) pattern)

value :: Int -> [String] -> Int
value smudges pattern = sum (axes smudges pattern) + 100 * sum (axes smudges $ transpose pattern)

part1 :: [[String]] -> Int
part1 = sum . map (value 0)

part2 :: [[String]] -> Int
part2 = sum . map (value 1)

main = do
    patterns <- map (lines . unpack) . splitOn (pack "\n\n") . pack <$> readFile "day13.txt"
    print $ part1 patterns
    print $ part2 patterns
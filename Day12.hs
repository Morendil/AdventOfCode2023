import Data.Text (pack, unpack, splitOn)
import Data.List (group)

type Record = (String, [Int])

parse :: String -> Record
parse s = (springs, runLengths)
    where [springs, runs] = words s
          runLengths = map (read . unpack) $ splitOn (pack ",") $ pack runs

arrangements :: Record -> Int
arrangements (springs, runLengths) = length $ filter (match runLengths) candidates
    where match lengths candidate = lengths == map length (filter ('#' `elem`) $ group candidate)
          candidates = mapM (\c -> if c == '?' then ['.','#'] else [c]) springs

part1 :: [Record] -> Int
part1 = sum . map arrangements

main = do
    records <- map parse . lines <$> readFile "day12.txt"
    print $ part1 records
import Data.Text (pack, unpack, splitOn)
import Data.List (intersect)
type Card = ([Int], [Int])

parseCard :: String -> Card
parseCard s = asPair nums
    where payload = last $ splitOn (pack ":") $ pack s
          leftRight = splitOn (pack " | ") payload
          nums = map (map read . words . unpack) leftRight
          asPair [x,y] = (x,y)

value :: Card -> Int
value (winning, owned) = if winners > 0 then 2 ^ (winners-1) else 0
    where winners = length (winning `intersect` owned)

part1 :: [Card] -> Int
part1 = sum . map value

main = do
    cards <- map parseCard . lines <$> readFile "day04.txt"
    print $ part1 cards
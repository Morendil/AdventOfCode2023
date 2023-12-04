import Data.Text (pack, unpack, splitOn)
import Data.List (intersect)
type Card = ([Integer], [Integer])

parseCard :: String -> Card
parseCard s = asPair nums
    where payload = last $ splitOn (pack ":") $ pack s
          leftRight = splitOn (pack " | ") payload
          nums = map (map read . words . unpack) leftRight
          asPair [x,y] = (x,y)

value :: Card -> Integer
value card = if winners > 0 then 2 ^ (winners-1) else 0
    where winners = matches card

matches :: Card -> Integer
matches(winning, owned) = toInteger $ length (winning `intersect` owned)

part1 :: [Card] -> Integer
part1 = sum . map value

step :: (Int, ([Integer], [Integer])) -> (Int, ([Integer], [Integer]))
step (card,(matches,counts)) = (card+1, (matches, counts'))
    where counts' = zipWith (+) counts copies
          copies = replicate (card+1) 0 ++ replicate (fromInteger $ matches !! card) (counts !! card) ++ repeat 0

part2 :: [Card] -> Integer
part2 cards = sum $ snd $ snd $ last $ take (count+1) $ iterate step (0,(matched,replicate count 1))
    where matched = map matches cards
          count = length cards

main = do
    cards <- map parseCard . lines <$> readFile "day04.txt"
    print $ part1 cards
    print $ part2 cards
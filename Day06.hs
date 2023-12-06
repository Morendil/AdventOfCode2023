import Data.Text (splitOn, pack, unpack)
import Data.Tuple.Extra

waysToBeat duration record = floor (root2-epsilon) - ceiling (root1+epsilon) + 1
    where (root1, root2) = roots duration record
          -- Just making the record isn't beating it !
          epsilon = 0.0000001

roots duration record = (root1, root2)
    where discriminant = sqrt $ fromIntegral ((duration ^ 2) - 4 * record)
          root1 = (fromIntegral duration - discriminant) / 2
          root2 = (discriminant + fromIntegral duration) / 2

part1 :: [(Integer,Integer)] -> Integer
part1 = product . map (uncurry waysToBeat)

part2 :: [(Integer,Integer)] -> Integer
part2 = part1 . (:[]) . unkern

unkern :: [(Integer,Integer)] -> (Integer, Integer)
unkern = both (read . concatMap show) . unzip

parsePairs :: String -> [(Integer, Integer)]
parsePairs = uncurry zip . pairUp . map ints . lines
    where ints :: String -> [Integer]
          ints = (map read . words . unpack . last) . splitOn (pack ":") . pack
          pairUp [a,b] = (a,b)

main = do
    races <- parsePairs <$> readFile "day06.txt"
    print $ part1 races
    print $ part2 races